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
            time(|| compile(&input, false));
        }

        Some(_) => eprintln!("Unrecognized benchmark name"),

        None => eprintln!("Please provide a benchmark name"),
    }
}

fn compile(input: &str, should_print: bool) {
    mem_usage("base", should_print);
    let world_index = hir::WorldIndex::default();
    let tokens = lexer::lex(input);
    mem_usage("lexed", should_print);
    let tree = parser::parse_repl_line(&tokens, input).into_syntax_tree();
    mem_usage("parsed", should_print);
    let root = ast::Root::cast(tree.root(), &tree).unwrap();
    mem_usage("got ast", should_print);
    let _diagnostics = ast::validation::validate(root, &tree);
    mem_usage("validated", should_print);
    let (index, _diagnostics) = hir::index(root, &tree, &world_index);
    mem_usage("indexed", should_print);
    let (bodies, _diagnostics) = hir::lower(root, &tree, &index, &world_index);
    mem_usage("lowered", should_print);
    let (_inference, _diagnostics) = hir_ty::infer_all(&bodies, &index, &world_index);
    mem_usage("inferred", should_print);
}

fn time(f: impl Fn()) {
    let mut times = [Duration::default(); 10];

    for time in &mut times {
        let now = Instant::now();
        f();
        *time = now.elapsed();
    }

    println!("took {:?}", times.iter().sum::<Duration>() / times.len() as u32);
}

fn mem_usage(stage: &str, should_print: bool) {
    if should_print {
        println!("{stage:>20}: {}MB", GLOBAL.total_size.load(Ordering::SeqCst) / 1_000_000);
    }
}
