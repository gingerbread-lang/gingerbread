use ast::AstNode;
use interner::Interner;
use rustc_hash::FxHashMap;
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
            compile(
                r#"
                        fnc add(x: s32, y: s32): s32 -> x + y;
                        fnc main: s32 -> add 10, 20 + 30;
                    "#,
                true,
                5_000_000,
                true,
            );
        }

        Some("long") => {
            let input = gen::gen(16 << 10 << 10); // 16 MiB
            compile(&input, false, 20, true);
        }

        Some(_) => eprintln!("Unrecognized benchmark name"),

        None => eprintln!("Please provide a benchmark name"),
    }
}

fn compile(input: &str, should_compile: bool, runs: usize, should_print: bool) {
    if should_print {
        println!("{} lines, {} bytes", input.lines().count(), input.len());
    }

    let mut previous_mem_usage = GLOBAL.total_size.load(Ordering::SeqCst);

    let tokens = stage("lex", || lexer::lex(input), &mut previous_mem_usage, runs, should_print);

    let mut world_index = hir::WorldIndex::default();

    let tree = stage(
        "parse",
        || parser::parse_repl_line(&tokens, input).into_syntax_tree(),
        &mut previous_mem_usage,
        runs,
        should_print,
    );

    let root = stage(
        "get ast",
        || ast::Root::cast(tree.root(), &tree).unwrap(),
        &mut previous_mem_usage,
        runs,
        should_print,
    );

    let _diagnostics = stage(
        "validate",
        || ast::validation::validate(root, &tree),
        &mut previous_mem_usage,
        runs,
        should_print,
    );

    let mut interner = Interner::default();

    let (index, _diagnostics) = stage(
        "index",
        || hir::index(root, &tree, &world_index, &mut interner),
        &mut previous_mem_usage,
        runs,
        should_print,
    );

    let (bodies, _diagnostics) = stage(
        "lower",
        || hir::lower(root, &tree, &index, &world_index, &mut interner),
        &mut previous_mem_usage,
        runs,
        should_print,
    );

    let (inference, _diagnostics) = stage(
        "infer",
        || hir_ty::infer_all(&bodies, &index, &world_index),
        &mut previous_mem_usage,
        runs,
        should_print,
    );

    if should_compile {
        let main = hir::Name(interner.intern("main"));
        world_index.add_module(main, index);

        let mut bodies_map = FxHashMap::default();
        let mut tys_map = FxHashMap::default();
        bodies_map.insert(main, bodies);
        tys_map.insert(main, inference);

        let _wasm = stage(
            "compile",
            || {
                eval::compile(
                    hir::Fqn { module: main, function: main },
                    bodies_map.clone(),
                    tys_map.clone(),
                    world_index.clone(),
                )
            },
            &mut previous_mem_usage,
            runs,
            should_print,
        );
    }
}

fn stage<T>(
    name: &str,
    mut f: impl FnMut() -> T,
    previous_mem_usage: &mut usize,
    runs: usize,
    should_print: bool,
) -> T {
    if !should_print {
        return f();
    }

    print!("{name:10}");

    let mut times = vec![Duration::default(); runs];
    let mut result = None;

    for time in &mut times {
        let now = Instant::now();
        result = Some(f());
        *time = now.elapsed();
    }

    print!("{:>15?}", times.iter().sum::<Duration>() / times.len() as u32);
    drop(times);

    let mem_usage = GLOBAL.total_size.load(Ordering::SeqCst);
    print!("{:5}MB", (mem_usage - *previous_mem_usage) / 1_000_000);
    print!("{:5}MB", mem_usage / 1_000_000);
    *previous_mem_usage = mem_usage;

    println!();

    result.unwrap()
}
