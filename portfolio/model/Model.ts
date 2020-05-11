import { ReactNode } from 'react'

export interface Portfolio {
  gigs: Gig[]
}

export interface Gig {
  title: string
  when: [string, string]
  note?: string
  projects: Project[]
}

export interface Project {
  title?: string
  description: ReactNode[]
  visuals: Visual[]
}

export interface Visual {
  img: string
  description: string
}
