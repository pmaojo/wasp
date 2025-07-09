{{={= =}=}}
import React from 'react'
import ReactDOM from 'react-dom/client'
import { QueryClientProvider } from '@tanstack/react-query'

import { Router } from './router'
import { HelmetProvider } from 'react-helmet-async'
import {
  initializeQueryClient,
  queryClientInitialized,
} from 'wasp/client/operations'

{=# setupFn.isDefined =}
{=& setupFn.importStatement =}
{=/ setupFn.isDefined =}

{=# areWebSocketsUsed =}
import { WebSocketProvider } from 'wasp/client/webSocket/WebSocketProvider'
{=/ areWebSocketsUsed =}

startApp()

async function startApp() {
  {=# setupFn.isDefined =}
  await {= setupFn.importIdentifier =}()
  {=/ setupFn.isDefined =}
  initializeQueryClient()

  await render()
}

async function render() {
  const queryClient = await queryClientInitialized
  ReactDOM.createRoot(document.getElementById('root') as HTMLElement).render(
    <React.StrictMode>
      <HelmetProvider>
        <QueryClientProvider client={queryClient}>
          {=# areWebSocketsUsed =}
          <WebSocketProvider>
            <Router />
          </WebSocketProvider>
          {=/ areWebSocketsUsed =}
          {=^ areWebSocketsUsed =}
          <Router />
          {=/ areWebSocketsUsed =}
        </QueryClientProvider>
      </HelmetProvider>
    </React.StrictMode>
  )
}
