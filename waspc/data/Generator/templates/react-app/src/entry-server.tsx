{{={= =}=}}
import React from 'react'
import { renderToString } from 'react-dom/server'
import { StaticRouter } from 'react-router-dom/server'
import { HelmetProvider } from 'react-helmet-async'
import { QueryClientProvider } from '@tanstack/react-query'
import { Router } from './router'
import { initializeQueryClient, queryClientInitialized } from 'wasp/client/operations'
{=# setupFn.isDefined =}
{=& setupFn.importStatement =}
{=/ setupFn.isDefined =}
{=# areWebSocketsUsed =}
import { WebSocketProvider } from 'wasp/client/webSocket/WebSocketProvider'
{=/ areWebSocketsUsed =}

export async function render(url: string) {
  {=# setupFn.isDefined =}
  await {= setupFn.importIdentifier =}()
  {=/ setupFn.isDefined =}
  initializeQueryClient()
  const queryClient = await queryClientInitialized
  const helmetContext: any = {}
  const html = renderToString(
    <React.StrictMode>
      <HelmetProvider context={helmetContext}>
        <QueryClientProvider client={queryClient}>
          {=# areWebSocketsUsed =}
          <WebSocketProvider>
            <StaticRouter location={url}>
              <Router />
            </StaticRouter>
          </WebSocketProvider>
          {=/ areWebSocketsUsed =}
          {=^ areWebSocketsUsed =}
          <StaticRouter location={url}>
            <Router />
          </StaticRouter>
          {=/ areWebSocketsUsed =}
        </QueryClientProvider>
      </HelmetProvider>
    </React.StrictMode>
  )
  return { html, helmet: helmetContext.helmet }
}
