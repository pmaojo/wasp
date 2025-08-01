import CliRunList from "../components/CliRunList";
import CliRunner from "../components/CliRunner";
import UserList from "../components/UserList";

export default function Home() {
  return (
    <>
      <UserList />
      <CliRunner />
      <CliRunList />
    </>
  );
}
