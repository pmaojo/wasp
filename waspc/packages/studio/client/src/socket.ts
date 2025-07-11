import { useEffect, useState } from "react";
import { io } from "socket.io-client";
import { StudioData } from "./types";

export const socket = io("http://localhost:4000", { autoConnect: false });

export function useSocket() {
  const [isConnected, setIsConnected] = useState(false);

  const [data, setData] = useState<StudioData | null>(null);

  function onData(data: string) {
    try {
      setData(JSON.parse(data) as StudioData);
    } catch (e: unknown) {
      console.error(e);
    }
  }

  function onConnect() {
    setIsConnected(true);
  }

  function onDisconnect() {
    setIsConnected(false);
  }

  useEffect(() => {
    socket.connect();
    socket.on("connect", onConnect);
    socket.on("disconnect", onDisconnect);
    socket.on("data", onData);
    return () => {
      socket.off("data", onData);
      socket.off("connect", onConnect);
      socket.off("disconnect", onDisconnect);
      socket.disconnect();
    };
  }, []);

  return {
    data,
    socket,
    isConnected,
  };
}
