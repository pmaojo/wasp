import type { NextApiRequest, NextApiResponse } from "next";
import { getRuns } from "../../services/cliService";

export default async function handler(
  _req: NextApiRequest,
  res: NextApiResponse,
) {
  const runs = await getRuns();
  res.status(200).json(runs);
}
