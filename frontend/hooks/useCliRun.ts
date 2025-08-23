import { useEffect, useState } from "react";
import { CliRun } from "../lib/cliRunStore";

export default function useCliRun(id: string) {
  const [run, setRun] = useState<CliRun | null>(null);

  useEffect(() => {
    if (!id) return;
    fetch(`/api/cli-runs/${id}`)
      .then((res) => (res.ok ? res.json() : null))
      .then((data) => setRun(data));
  }, [id]);

  return { run };
}
