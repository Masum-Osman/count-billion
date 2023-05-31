import java.util.ArrayList;
import java.util.List;

class ThreadData {
    public int start;
    public int end;
    public int count;

    public ThreadData(int start, int end) {
        this.start = start;
        this.end = end;
        this.count = 0;
    }
}

class NumberCounter implements Runnable {
    private ThreadData data;

    public NumberCounter(ThreadData data) {
        this.data = data;
    }

    @Override
    public void run() {
        data.count = 0;
        for (int i = data.start; i <= data.end; i++) {
            data.count++;
        }
    }
}

public class Main {
    private static final int NUM_CPU = Runtime.getRuntime().availableProcessors();
    private static final int NUM_NUMBERS = 1000000000;

    public static void main(String[] args) {
        int numCPU = NUM_CPU;
        int perChunk = NUM_NUMBERS / numCPU;
        int remainder = NUM_NUMBERS % numCPU;

        List<ThreadData> threadData = new ArrayList<>();
        List<Thread> threads = new ArrayList<>();

        long startTime = System.currentTimeMillis();

        for (int i = 0; i < numCPU; i++) {
            int start = i * perChunk + 1;
            int end = (i == numCPU - 1) ? start + perChunk + remainder - 1 : start + perChunk - 1;
            ThreadData data = new ThreadData(start, end);
            threadData.add(data);
            Thread t = new Thread(new NumberCounter(data));
            threads.add(t);
            t.start();
        }

        int totalCount = 0;
        for (Thread t : threads) {
            try {
                t.join();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
            totalCount += t.data.count;
        }

        long endTime = System.currentTimeMillis();
        long executionTime = endTime - startTime;

        System.out.println("Count: " + totalCount);
        System.out.println("Execution Time: " + executionTime + " milliseconds");
    }
}
