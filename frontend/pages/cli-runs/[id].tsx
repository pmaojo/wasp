import { useRouter } from "next/router";
import CliRunDetail from "../../components/CliRunDetail";

export default function CliRunPage() {
  const { query } = useRouter();
  const id = typeof query.id === "string" ? query.id : "";
  if (!id) return null;
  return <CliRunDetail id={id} />;
}
