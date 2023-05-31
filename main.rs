use std::sync::mpsc;
use std::thread;
use std::time::Instant;

fn count_numbers(start: i32, end: i32, sender: mpsc::Sender<i32>) {
    let mut count = 0;
    for i in start..=end {
        count += 1;
    }
    sender.send(count).unwrap();
}

fn main() {
    let num_cpu = num_cpus::get();
    let (sender, receiver) = mpsc::channel();

    let per_chunk = 1_000_000_000 / num_cpu;
    let remainder = 1_000_000_000 % num_cpu;

    let start_time = Instant::now();

    for i in 0..num_cpu {
        let sender = sender.clone();
        let start = i * per_chunk + 1;
        let end = if i == num_cpu - 1 {
            start + per_chunk + remainder - 1
        } else {
            start + per_chunk - 1
        };

        thread::spawn(move || {
            count_numbers(start, end, sender);
        });
    }

    let mut total_count = 0;
    for _ in 0..num_cpu {
        let count = receiver.recv().unwrap();
        total_count += count;
    }

    let end_time = Instant::now();
    let execution_time = end_time - start_time;

    println!("Count: {}", total_count);
    println!("Execution Time: {:?}", execution_time);
}
