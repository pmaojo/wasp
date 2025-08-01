import { useState } from "react";
import useCli from "../hooks/useCli";

export default function CliRunner() {
  const { jobId, run } = useCli();
  const [cmd, setCmd] = useState("");

  return (
    <div>
      <input value={cmd} onChange={(e) => setCmd(e.target.value)} />
      <button onClick={() => run(cmd)}>Run</button>
      {jobId && <p>Job started: {jobId}</p>}
    </div>
  );
}
