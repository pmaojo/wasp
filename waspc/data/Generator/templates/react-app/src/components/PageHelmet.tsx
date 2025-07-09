{{={= =}=}}
import React from 'react'
import { Helmet } from 'react-helmet-async'

export type MetaTag = {
  name: string
  content: string
}

export interface PageHead {
  title?: string
  meta?: MetaTag[]
}

/**
 * Renders document head elements using react-helmet-async.
 * Helps decouple pages from head management for easier testing.
 */
export function PageHelmet({ title, meta = [] }: PageHead) {
  return (
    <Helmet>
      {title && <title>{title}</title>}
      {meta.map((m) => (
        <meta key={m.name} name={m.name} content={m.content} />
      ))}
    </Helmet>
  )
}
