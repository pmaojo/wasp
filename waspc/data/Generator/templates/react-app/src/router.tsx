{{={= =}=}}
import React from 'react'
import { createBrowserRouter, RouterProvider, useMatches } from 'react-router-dom'
import { PageHelmet, type PageHead } from './components/PageHelmet'
{=# rootComponent.isDefined =}
{=& rootComponent.importStatement =}
{=/ rootComponent.isDefined =}

{=# isAuthEnabled =}
import createAuthRequiredPage from "./auth/pages/createAuthRequiredPage"
{=/ isAuthEnabled =}

{=# pagesToImport =}
{=& importStatement =}
{=/ pagesToImport =}

{=# isExternalAuthEnabled =}
import { OAuthCallbackPage } from "./auth/pages/OAuthCallback"
{=/ isExternalAuthEnabled =}

import { DefaultRootErrorBoundary } from './components/DefaultRootErrorBoundary'

import { routes } from 'wasp/client/router'

export const routeNameToRouteComponent = {
  {=# routes =}
  {= name =}: {= targetComponent =},
  {=/ routes =}
} as const;

const waspDefinedRoutes = [
  {=# isExternalAuthEnabled =}
  {
    path: "{= oAuthCallbackPath =}",
    Component: OAuthCallbackPage,
  },
  {=/ isExternalAuthEnabled =}
]
const userDefinedRoutes = Object.entries(routes).map(([routeKey, route]) => {
  return {
    path: route.to,
    Component: routeNameToRouteComponent[routeKey],
    handle: { head: (route as { head?: PageHead }).head },
  }
})

const browserRouter = createBrowserRouter([
  {
    path: '/',
    {=# rootComponent.isDefined =}
    element: <{= rootComponent.importIdentifier =} />,
    {=/ rootComponent.isDefined =}
    ErrorBoundary: DefaultRootErrorBoundary,
    children: [
      ...waspDefinedRoutes,
      ...userDefinedRoutes,
    ],
  },
])

export function Router() {
  const matches = useMatches() as { handle?: { head?: PageHead } }[]
  const heads = matches.flatMap((m) => (m.handle?.head ? [m.handle.head] : []))
  return (
    <>
      {heads.map((h, i) => (h ? <PageHelmet key={i} {...h} /> : null))}
      <RouterProvider router={browserRouter} />
    </>
  )
}
