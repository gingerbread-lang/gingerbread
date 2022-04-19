use ast::AstNode;
use std::alloc::GlobalAlloc;
use std::env;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::time::{Duration, Instant};

#[global_allocator]
static GLOBAL: TrackingAlloc =
    TrackingAlloc { total_size: AtomicUsize::new(0), mimalloc: mimalloc::MiMalloc };

struct TrackingAlloc {
    total_size: AtomicUsize,
    mimalloc: mimalloc::MiMalloc,
}

unsafe impl GlobalAlloc for TrackingAlloc {
    unsafe fn alloc(&self, layout: std::alloc::Layout) -> *mut u8 {
        self.total_size.fetch_add(layout.size(), Ordering::SeqCst);
        self.mimalloc.alloc(layout)
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: std::alloc::Layout) {
        self.total_size.fetch_sub(layout.size(), Ordering::SeqCst);
        self.mimalloc.dealloc(ptr, layout)
    }

    unsafe fn alloc_zeroed(&self, layout: std::alloc::Layout) -> *mut u8 {
        self.total_size.fetch_add(layout.size(), Ordering::SeqCst);
        self.mimalloc.alloc_zeroed(layout)
    }

    unsafe fn realloc(&self, ptr: *mut u8, layout: std::alloc::Layout, new_size: usize) -> *mut u8 {
        self.total_size.fetch_sub(layout.size(), Ordering::SeqCst);
        self.total_size.fetch_add(new_size, Ordering::SeqCst);
        self.mimalloc.realloc(ptr, layout, new_size)
    }
}

fn main() {
    let mut args = env::args();
    args.next();

    match args.next().as_deref() {
        Some("parser") => {
            let input = gen::gen(4 << 10 << 10); // 4 MiB
            let tokens = lexer::lex(&input);
            parser::parse_source_file(&tokens, &input);
        }

        Some("short") => {
            for _ in 0..100_000 {
                compile(
                    r#"
                        fnc add(x: s32, y: s32): s32 -> x + y;
                        fnc main -> add 10, 20 + 30;
                    "#,
                    false,
                );
            }
        }

        Some("long") => {
            let input = gen::gen(16 << 10 << 10); // 16 MiB
            compile(&input, true);
        }

        Some(_) => eprintln!("Unrecognized benchmark name"),

        None => eprintln!("Please provide a benchmark name"),
    }
}

fn compile(input: &str, should_print: bool) {
    let mut previous_mem_usage = GLOBAL.total_size.load(Ordering::SeqCst);

    let tokens = stage("lex", || lexer::lex(input), &mut previous_mem_usage, should_print);

    let world_index = hir::WorldIndex::default();

    let tree = stage(
        "parse",
        || parser::parse_repl_line(&tokens, input).into_syntax_tree(),
        &mut previous_mem_usage,
        should_print,
    );

    let root = stage(
        "get ast",
        || ast::Root::cast(tree.root(), &tree).unwrap(),
        &mut previous_mem_usage,
        should_print,
    );

    let _diagnostics = stage(
        "validate",
        || ast::validation::validate(root, &tree),
        &mut previous_mem_usage,
        should_print,
    );

    let (index, _diagnostics) = stage(
        "index",
        || hir::index(root, &tree, &world_index),
        &mut previous_mem_usage,
        should_print,
    );

    let (bodies, _diagnostics) = stage(
        "lower",
        || hir::lower(root, &tree, &index, &world_index),
        &mut previous_mem_usage,
        should_print,
    );

    let (_inference, _diagnostics) = stage(
        "infer",
        || hir_ty::infer_all(&bodies, &index, &world_index),
        &mut previous_mem_usage,
        should_print,
    );
}

fn stage<T>(
    name: &str,
    f: impl Fn() -> T,
    previous_mem_usage: &mut usize,
    should_print: bool,
) -> T {
    if !should_print {
        return f();
    }

    print!("{name:10}");

    let mut times = [Duration::default(); 20];
    let mut result = None;

    for time in &mut times {
        let now = Instant::now();
        result = Some(f());
        *time = now.elapsed();
    }

    print!("{:>15?}", times.iter().sum::<Duration>() / times.len() as u32);

    let mem_usage = GLOBAL.total_size.load(Ordering::SeqCst);
    print!("{:5}MB", (mem_usage - *previous_mem_usage) / 1_000_000);
    print!("{:5}MB", mem_usage / 1_000_000);
    *previous_mem_usage = mem_usage;

    println!();

    result.unwrap()
}
