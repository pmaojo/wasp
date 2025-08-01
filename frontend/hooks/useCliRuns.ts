import { useEffect, useState } from "react";
import { CliRun } from "../lib/cliRunStore";

export default function useCliRuns() {
  const [runs, setRuns] = useState<CliRun[]>([]);

  useEffect(() => {
    fetch("/api/cli-runs")
      .then((res) => res.json())
      .then(setRuns);
  }, []);

  return { runs };
}
