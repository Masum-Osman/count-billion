const { Worker, isMainThread, parentPort, workerData } = require('worker_threads');

function countNumbers(start, end) {
  let count = 0;
  for (let i = start; i <= end; i++) {
    count++;
  }
  return count;
}

if (isMainThread) {
  const numCPU = require('os').cpus().length;
  const perChunk = Math.floor(1000000000 / numCPU);
  const remainder = 1000000000 % numCPU;
  const start = 1;

  const startTime = new Date();

  let totalCount = 0;
  let completedWorkers = 0;

  function handleWorkerCount(count) {
    totalCount += count;
    completedWorkers++;

    if (completedWorkers === numCPU) {
      const endTime = new Date();
      const executionTime = endTime - startTime;

      console.log('Count:', totalCount);
      console.log('Execution Time:', executionTime);
    }
  }

  for (let i = 0; i < numCPU; i++) {
    const workerStart = start + i * perChunk;
    const workerEnd = i === numCPU - 1 ? workerStart + perChunk + remainder - 1 : workerStart + perChunk - 1;

    const worker = new Worker(__filename, { workerData: { start: workerStart, end: workerEnd } });
    worker.on('message', handleWorkerCount);
  }
} else {
  const { start, end } = workerData;
  const count = countNumbers(start, end);
  parentPort.postMessage(count);
}
