import { useState } from "react";

export interface CliResult {
  jobId: string;
}

export default function useCli() {
  const [jobId, setJobId] = useState<string | null>(null);

  const run = (cmd: string) =>
    fetch(`/Cli?cmd=${encodeURIComponent(cmd)}`)
      .then((res) => res.json())
      .then((data: CliResult) => setJobId(data.jobId));

  return { jobId, run };
}
