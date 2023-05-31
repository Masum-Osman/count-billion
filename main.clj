use std::sync::{mpsc, Arc, Mutex};
use std::thread;
use std::time::Instant;

fn count_numbers(start: i32, end: i32) -> i32 {
    let mut count = 0;
    for i in start..=end {
        count += 1;
    }
    count
}

fn main() {
    let num_cpu = num_cpus::get();
    let per_chunk = 1_000_000_000 / num_cpu;
    let remainder = 1_000_000_000 % num_cpu;

    let start_time = Instant::now();

    let total_count = Arc::new(Mutex::new(0));

    let (tx, rx) = mpsc::channel();

    for i in 0..num_cpu {
        let start = i * per_chunk + 1;
        let end = if i == num_cpu - 1 {
            start + per_chunk + remainder - 1
        } else {
            start + per_chunk - 1
        };

        let tx = tx.clone();
        let total_count = Arc::clone(&total_count);

        thread::spawn(move || {
            let count = count_numbers(start, end);
            tx.send(count).unwrap();
            let mut total_count = total_count.lock().unwrap();
            *total_count += count;
        });
    }

    drop(tx);

    let mut final_count = 0;
    for count in rx {
        final_count += count;
    }

    let end_time = Instant::now();
    let execution_time = end_time - start_time;

    println!("Count: {}", final_count);
    println!("Execution Time: {:?}", execution_time);
}
