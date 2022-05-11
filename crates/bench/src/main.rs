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
            Bench::new(
                r#"
                    fnc add(x: s32, y: s32): s32 -> x + y;
                    fnc main: s32 -> add 10, 20 + 30;
                "#,
                BenchOptions { should_compile: true, runs: 5_000_000 },
            )
            .run();
        }

        Some("long") => {
            let input = gen::gen(16 << 10 << 10); // 16 MiB
            Bench::new(&input, BenchOptions { should_compile: false, runs: 20 }).run();
        }

        Some(_) => eprintln!("Unrecognized benchmark name"),

        None => eprintln!("Please provide a benchmark name"),
    }
}

struct BenchOptions {
    should_compile: bool,
    runs: usize,
}

struct Bench<'a> {
    input: &'a str,
    options: BenchOptions,
    initial_mem_usage: usize,
    previous_mem_usage: usize,
    total_time: Duration,
}

impl<'a> Bench<'a> {
    #[must_use]
    fn new(input: &'a str, options: BenchOptions) -> Self {
        let mem_usage = GLOBAL.total_size.load(Ordering::SeqCst);
        Self {
            input,
            options,
            initial_mem_usage: mem_usage,
            previous_mem_usage: 0,
            total_time: Duration::default(),
        }
    }

    fn run(mut self) {
        println!("{} lines, {} bytes", self.input.lines().count(), self.input.len());

        let tokens = self.stage("lex", || lexer::lex(self.input));

        let mut world_index = hir::WorldIndex::default();

        let tree =
            self.stage("parse", || parser::parse_repl_line(&tokens, self.input).into_syntax_tree());

        let root = self.stage("get ast", || ast::Root::cast(tree.root(), &tree).unwrap());

        let _diagnostics = self.stage("validate", || ast::validation::validate(root, &tree));

        let mut interner = Interner::default();

        let (index, _diagnostics) =
            self.stage("index", || hir::index(root, &tree, &world_index, &mut interner));

        let (bodies, _diagnostics) =
            self.stage("lower", || hir::lower(root, &tree, &index, &world_index, &mut interner));

        let (inference, _diagnostics) =
            self.stage("infer", || hir_ty::infer_all(&bodies, &index, &world_index));

        if self.options.should_compile {
            let main = hir::Name(interner.intern("main"));
            world_index.add_module(main, index);

            let mut bodies_map = FxHashMap::default();
            let mut tys_map = FxHashMap::default();
            bodies_map.insert(main, bodies);
            tys_map.insert(main, inference);

            let _wasm = self.stage("compile", || {
                eval::compile(
                    hir::Fqn { module: main, name: main },
                    bodies_map.clone(),
                    tys_map.clone(),
                    &world_index,
                )
            });
        }

        // MB/s == B/µs
        let throughput = self.input.len() as f32 / self.total_time.as_micros() as f32;
        let lines_per_second = self.input.lines().count() as f32 / self.total_time.as_secs_f32();
        println!();
        println!("{:?}", self.total_time);
        println!("{throughput} MB/s");
        println!("{:.0} KLOC/s", lines_per_second / 1000.0);
    }

    fn stage<T>(&mut self, name: &str, mut f: impl FnMut() -> T) -> T {
        print!("{name:10}");

        let mut times = vec![Duration::default(); self.options.runs];
        let mut result = None;

        for time in &mut times {
            let now = Instant::now();
            result = Some(f());
            *time = now.elapsed();
        }

        let average_time = times.iter().sum::<Duration>() / times.len() as u32;
        self.total_time += average_time;
        print!("{:>15?}", average_time);

        drop(times);

        let mem_usage = GLOBAL.total_size.load(Ordering::SeqCst) - self.initial_mem_usage;
        print!("{:5}MB", (mem_usage - self.previous_mem_usage) / 1_000_000);
        print!("{:5}MB", mem_usage / 1_000_000);
        self.previous_mem_usage = mem_usage;

        println!();

        result.unwrap()
    }
}
