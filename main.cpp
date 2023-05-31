#include <iostream>
#include <thread>
#include <vector>
#include <chrono>

constexpr int NUM_CPU = std::thread::hardware_concurrency();
constexpr int NUM_NUMBERS = 1000000000;

struct ThreadData {
    int start;
    int end;
    int count;
};

void countNumbers(ThreadData* data) {
    data->count = 0;
    for (int i = data->start; i <= data->end; i++) {
        data->count++;
    }
}

int main() {
    int numCPU = NUM_CPU;
    int perChunk = NUM_NUMBERS / numCPU;
    int remainder = NUM_NUMBERS % numCPU;

    std::vector<ThreadData> threadData(numCPU);

    std::vector<std::thread> threads;

    auto startTime = std::chrono::steady_clock::now();

    for (int i = 0; i < numCPU; i++) {
        threadData[i].start = i * perChunk + 1;
        threadData[i].end = (i == numCPU - 1) ? threadData[i].start + perChunk + remainder - 1 : threadData[i].start + perChunk - 1;
        threads.emplace_back(countNumbers, &threadData[i]);
    }

    int totalCount = 0;
    for (int i = 0; i < numCPU; i++) {
        threads[i].join();
        totalCount += threadData[i].count;
    }

    auto endTime = std::chrono::steady_clock::now();
    auto executionTime = std::chrono::duration_cast<std::chrono::microseconds>(endTime - startTime).count();

    std::cout << "Count: " << totalCount << std::endl;
    std::cout << "Execution Time: " << executionTime << " microseconds" << std::endl;

    return 0;
}
