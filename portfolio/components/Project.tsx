import React from 'react'
import { Project, Gig } from '../model/Model'
import { VisualComponent } from './Visual'

interface Props {
  gig: Gig
  project: Project
}

const Styling = () => (
  <style jsx>{`
    .image-grid {
      display: flex;
      flex-wrap: wrap;
      justify-content: space-around;
    }
    .image-grid a {
      text-decoration: none;
      margin: 10px;
      width: 278px;
    }
    .overview {
      padding: 20px 0 60px 0;
      background: white;
      font-size: 12px;
    }
    .thumb {
      outline: solid 2px #f4f4f0;
      width: 278px;
      height: 278px;
    }
    .image-grid a:hover .thumb {
      outline: solid 2px #2196f3;
    }
    .image-grid a:hover .overview {
      opacity: 1;
    }
  `}</style>
)

export function ProjectComponent(props: Props) {
  const { project, gig } = props
  const { title, description, visuals } = project
  return (
    <>
      <Styling />
      <li id={title}>
        {title && <h3>{title}</h3>}
        <div className='desc'>
          {description.map((desc, ix) => (
            <p key={ix}>{desc}</p>
          ))}
        </div>
        <div className='image-grid'>
          {visuals.map((visual, ix) => (
            <VisualComponent
              key={ix}
              gig={gig}
              visual={visual}
            ></VisualComponent>
          ))}
        </div>
      </li>
    </>
  )
}
