package main

import (
	"fmt"
	"runtime"
	"sync"
	"time"
)

// func countNumbers(start, end int, ch chan int) {
// 	count := 0
// 	for i := start; i <= end; i++ {
// 		count++
// 	}
// 	ch <- count
// }

func countNumbers(start, end int, wg *sync.WaitGroup, ch chan int) {
	defer wg.Done()

	count := 0
	for i := start; i <= end; i++ {
		count++
	}
	ch <- count
}

func countBillionStraight() {
	count := 0
	for i := 1; i <= 1000000000; i++ {
		count++
	}
	fmt.Println("Count:", count)
}

func countBillionByAllCPUCores() {
	numCPU := runtime.NumCPU()
	perChunk := 1000000000 / numCPU
	remainder := 1000000000 % numCPU

	wg := sync.WaitGroup{}
	wg.Add(numCPU)

	ch := make(chan int, numCPU)

	startTime := time.Now()

	for i := 0; i < numCPU; i++ {
		start := i*perChunk + 1
		end := start + perChunk - 1

		if i == numCPU-1 {
			end += remainder
		}

		go countNumbers(start, end, &wg, ch)
	}

	go func() {
		wg.Wait()
		close(ch)
	}()

	totalCount := 0
	for count := range ch {
		totalCount += count
	}

	executionTime := time.Since(startTime)

	fmt.Println("Count:", totalCount)
	fmt.Println("Execution Time:", executionTime)
}

// func countBillionInTwoChannel() {
// 	ch := make(chan int)

// 	go countNumbers(1, 500000000, ch)
// 	go countNumbers(500000001, 1000000000, ch)

// 	count1 := <-ch
// 	count2 := <-ch

// 	totalCount := count1 + count2
// 	fmt.Println("Count:", totalCount)
// }

func main() {
	countBillionByAllCPUCores()
}
