import { render, screen } from "@testing-library/react";
import CliRunList from "../components/CliRunList";

jest.mock("../hooks/useCliRuns", () => () => ({
  runs: [
    {
      id: "1",
      command: "echo",
      args: ["hi"],
      stdout: "hi\n",
      stderr: "",
      exitCode: 0,
      createdAt: "",
    },
  ],
}));

test("renders cli runs", () => {
  render(<CliRunList />);
  expect(screen.getByText(/echo hi/)).toBeInTheDocument();
});
