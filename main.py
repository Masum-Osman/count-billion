import threading
import time

NUM_CPU = threading.cpu_count()
NUM_NUMBERS = 1000000000

class ThreadData:
    def __init__(self, start, end):
        self.start = start
        self.end = end
        self.count = 0

def count_numbers(data):
    data.count = 0
    for i in range(data.start, data.end + 1):
        data.count += 1

def main():
    num_cpu = NUM_CPU
    per_chunk = NUM_NUMBERS // num_cpu
    remainder = NUM_NUMBERS % num_cpu

    thread_data = []
    threads = []

    start_time = time.time()

    for i in range(num_cpu):
        start = i * per_chunk + 1
        end = start + per_chunk + remainder - 1 if i == num_cpu - 1 else start + per_chunk - 1
        data = ThreadData(start, end)
        thread_data.append(data)
        t = threading.Thread(target=count_numbers, args=(data,))
        threads.append(t)
        t.start()

    total_count = 0
    for t in threads:
        t.join()
        total_count += t.data.count

    end_time = time.time()
    execution_time = end_time - start_time

    print("Count:", total_count)
    print("Execution Time:", execution_time, "seconds")

if __name__ == '__main__':
    main()
