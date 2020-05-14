import { Portfolio } from '../model/Model'
import { Fragment } from 'react'

interface Props {
  portfolio: Portfolio
}

const Styling = () => (
  <style jsx>{`
    nav {
      max-width: 700px;
      margin: 30px auto;
    }
    nav {
      text-align: center;
    }
    nav > *:not(:last-child) {
      margin-right: 20px;
    }
  `}</style>
)

export const Nav = ({ portfolio }: Props) => (
  <>
    <Styling />
    <nav>
      {portfolio.gigs.map(({ title }) => (
        <Fragment key={title}>
          <b>
            <a href={'#' + title}>{title}</a>
          </b>
        </Fragment>
      ))}
    </nav>
  </>
)
