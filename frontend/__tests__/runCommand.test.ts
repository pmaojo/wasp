import { runCommand } from "../lib/runCommand";

test("captures stdout and exit code", async () => {
  const res = await runCommand("node", ["-e", "process.stdout.write('ok')"]);
  expect(res.stdout).toBe("ok");
  expect(res.code).toBe(0);
});

test("captures non-zero exit code", async () => {
  const res = await runCommand("node", ["-e", "process.exit(1)"]);
  expect(res.code).toBe(1);
});
