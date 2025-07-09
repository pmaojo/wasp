import { ExportsRequests, getExportsOfFiles } from "./exports.js";
import { OperationsRequests, getOperationsOfFiles } from "./operations.js";

async function readAllFromStdin(): Promise<string> {
  return new Promise((resolve, reject) => {
    let chunks = "";
    process.stdin.on("data", (data) => {
      chunks += data;
    });
    process.stdin.on("end", () => resolve(chunks));
    process.stdin.on("close", () => resolve(chunks));
    process.stdin.on("error", (err) => reject(err));
  });
}

async function main() {
  const inputStr = await readAllFromStdin();
  const input = JSON.parse(inputStr);

  const mode = process.argv[2] ?? "exports";
  if (mode === "operations") {
    const requests = OperationsRequests.parse(input);
    let ops: Record<string, string[]> = {};
    for (const request of requests) {
      const newOps = await getOperationsOfFiles(request);
      ops = { ...ops, ...newOps };
    }
    console.log(JSON.stringify(ops));
    return;
  }

  const requests = ExportsRequests.parse(input);
  let exports: Record<string, any[]> = {};
  for (const request of requests) {
    const newExports = await getExportsOfFiles(request);
    exports = { ...exports, ...newExports };
  }
  console.log(JSON.stringify(exports));
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});

