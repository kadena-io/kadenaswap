import { createGlobalStyle } from 'styled-components';

export default createGlobalStyle`
    *, *:before, *:after {
      -webkit-box-sizing: inherit;
      -moz-box-sizing: inherit;
      box-sizing: inherit;
    }

    *:focus {
      outline: none;
    }

    html {
      width: 100%;
      height: 100%;
      box-sizing: border-box;
    };

    body {
      margin: 0;
      width: 100%;
      height: 100%;
      line-height: inherit;
      overflow: auto;
      min-width: 0;
      color: ${({ theme: { colors } }) => colors.primary};
      background: ${() => `url(/images/background.jpg)`};
      background-color: #282c34;
      background-size: cover;
      background-repeat: no-repeat;
    };

    #root {
      height: 100%;

      & > div:first-child {
        display: flex;
        flex-flow: column;
        height: 100%;
      }
    }

    .ui.disabled.button {
      opacity: 1 !important;
    }

    .desktop-none {
      @media (min-width: ${({ theme: { mediaQueries } }) => `${mediaQueries.mobilePixel + 1}px`}) {
        display: none !important;
      }
    }

    .mobile-none {
      @media (max-width: ${({ theme: { mediaQueries } }) => `${mediaQueries.mobilePixel}px`}) {
        display: none !important;
      }
    }
`;
