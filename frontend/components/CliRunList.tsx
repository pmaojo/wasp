import useCliRuns from "../hooks/useCliRuns";

export default function CliRunList() {
  const { runs } = useCliRuns();
  return (
    <ul>
      {runs.map((r) => (
        <li key={r.id}>
          <code>
            {r.command} {r.args.join(" ")}
          </code>{" "}
          → {r.exitCode}
        </li>
      ))}
    </ul>
  );
}
