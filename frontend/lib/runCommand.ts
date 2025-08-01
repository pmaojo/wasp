import { execFile } from "child_process";
import { promisify } from "util";

const execFileAsync = promisify(execFile);

export interface CommandResult {
  stdout: string;
  stderr: string;
  code: number;
}

export async function runCommand(
  command: string,
  args: string[] = [],
): Promise<CommandResult> {
  try {
    const { stdout, stderr } = await execFileAsync(command, args);
    return { stdout, stderr, code: 0 };
  } catch (error: any) {
    return {
      stdout: error?.stdout ?? "",
      stderr: error?.stderr ?? String(error),
      code: typeof error?.code === "number" ? error.code : 1,
    };
  }
}
