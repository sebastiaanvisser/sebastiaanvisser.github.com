import React from 'react'
import { Visual, Gig } from '../model/Model'

interface Props {
  gig: Gig
  visual: Visual
}

export function VisualComponent(props: Props) {
  const { gig, visual } = props
  const { description } = visual
  const img = `/${gig.title.toLowerCase()}/${visual.img}.png`
  const thumb = `/${gig.title.toLowerCase()}/${visual.img} - thumb.png`
  return (
    <a href={img}>
      <img className='thumb' src={thumb} />
      <div className='overview'>{description}</div>
    </a>
  )
}
