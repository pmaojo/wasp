import useCliRun from "../hooks/useCliRun";

interface Props {
  id: string;
}

export default function CliRunDetail({ id }: Props) {
  const { run } = useCliRun(id);
  if (!run) return <p>Loading...</p>;

  return (
    <div>
      <h2>
        <code>{run.command} {run.args.join(" ")}</code>
      </h2>
      <p>Exit code: {run.exitCode}</p>
      <h3>Stdout</h3>
      <pre>{run.stdout}</pre>
      <h3>Stderr</h3>
      <pre>{run.stderr}</pre>
    </div>
  );
}
