import Link from "next/link";
import useCliRuns from "../hooks/useCliRuns";

export default function CliRunList() {
  const { runs } = useCliRuns();
  return (
    <ul>
      {runs.map((r) => (
        <li key={r.id}>
          <Link href={`/cli-runs/${r.id}`}>
            <code>
              {r.command} {r.args.join(" ")}
            </code>
          </Link>{" "}→ {r.exitCode}
        </li>
      ))}
    </ul>
  );
}
