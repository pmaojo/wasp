import { randomUUID } from "crypto";
import { promises as fs } from "fs";
import path from "path";

export interface CliRun {
  id: string;
  command: string;
  args: string[];
  stdout: string;
  stderr: string;
  exitCode: number;
  createdAt: string;
}

const DATA_PATH = path.join(process.cwd(), "data", "cli-runs.json");

async function readFile(): Promise<CliRun[]> {
  try {
    const content = await fs.readFile(DATA_PATH, "utf-8");
    return JSON.parse(content) as CliRun[];
  } catch {
    return [];
  }
}

async function writeFile(runs: CliRun[]): Promise<void> {
  await fs.mkdir(path.dirname(DATA_PATH), { recursive: true });
  await fs.writeFile(DATA_PATH, JSON.stringify(runs, null, 2), "utf-8");
}

export async function listRuns(): Promise<CliRun[]> {
  return readFile();
}

export async function recordRun(data: {
  command: string;
  args: string[];
  stdout: string;
  stderr: string;
  exitCode: number;
}): Promise<CliRun> {
  const run: CliRun = {
    id: randomUUID(),
    createdAt: new Date().toISOString(),
    ...data,
  };
  const runs = await readFile();
  runs.push(run);
  await writeFile(runs);
  return run;
}
