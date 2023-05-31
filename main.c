#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <sys/sysinfo.h>
#include <sys/time.h>

#define NUM_CPU get_nprocs()
#define NUM_NUMBERS 1000000000

struct ThreadData {
    int start;
    int end;
    int count;
};

void* countNumbers(void* arg) {
    struct ThreadData* data = (struct ThreadData*) arg;
    data->count = 0;
    for (int i = data->start; i <= data->end; i++) {
        data->count++;
    }
    return NULL;
}

int main() {
    int numCPU = NUM_CPU;
    int perChunk = NUM_NUMBERS / numCPU;
    int remainder = NUM_NUMBERS % numCPU;

    struct ThreadData* threadData = malloc(numCPU * sizeof(struct ThreadData));

    pthread_t* threads = malloc(numCPU * sizeof(pthread_t));
    
    struct timeval startTime, endTime;
    gettimeofday(&startTime, NULL);

    for (int i = 0; i < numCPU; i++) {
        threadData[i].start = i * perChunk + 1;
        threadData[i].end = (i == numCPU - 1) ? threadData[i].start + perChunk + remainder - 1 : threadData[i].start + perChunk - 1;
        pthread_create(&threads[i], NULL, countNumbers, &threadData[i]);
    }

    int totalCount = 0;
    for (int i = 0; i < numCPU; i++) {
        pthread_join(threads[i], NULL);
        totalCount += threadData[i].count;
    }

    gettimeofday(&endTime, NULL);
    long executionTime = (endTime.tv_sec - startTime.tv_sec) * 1000000 + (endTime.tv_usec - startTime.tv_usec);

    printf("Count: %d\n", totalCount);
    printf("Execution Time: %ld microseconds\n", executionTime);

    free(threadData);
    free(threads);

    return 0;
}
