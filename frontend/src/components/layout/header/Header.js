import React, { useContext, useState } from 'react';
import { NavLink, useHistory } from 'react-router-dom';
import { Button, Modal } from 'semantic-ui-react'
import styled from 'styled-components/macro';
import reduceToken from '../../../utils/reduceToken';
import { useWallet } from 'use-wallet'
import { Web3ReactProvider, useWeb3React, UnsupportedChainIdError } from '@web3-react/core'
import EthModal from '../../../modals/ethModal/EthModal';
import KdaModal from '../../../modals/kdaModal/KdaModal';
import { ROUTE_INDEX, ROUTE_POOL, ROUTE_SWAP, ROUTE_WRAP } from '../../../router/routes';
import { ReactComponent as KDALogo } from '../../../assets/images/header/kadena-logo.svg';
import { ReactComponent as PowerIcon } from '../../../assets/images/header/power.svg';
import { ReactComponent as CogIcon } from '../../../assets/images/header/cog.svg';
import { ReactComponent as HamburgerIcon } from '../../../assets/images/header/hamburger.svg';

const Container = styled.div`
  position: fixed;
  top: 0;
  left: 18px;
  display: flex;
  justify-content: space-between;
  min-height: ${({ theme: { header } }) => `${header.height}px`};
  width: calc(100% - 3em);

  @media (min-width: ${({ theme: { mediaQueries } }) => mediaQueries.mobileBreakpoint}) {
    width: inherit;
    left: unset;
  }
`;

const LeftContainer = styled.div`
  display: flex;
  align-items: center;
  margin-right: 25px;

  & > *:not(:last-child) {
    margin-right: 25px;
  }
`;

const RightContainer = styled.div`
  display: flex;
  align-items: center;

  & > *:first-child {
    margin-right: 13px;
  }

  & > *:not(:first-child):not(:last-child) {
    margin-right: 14px;
  }

  @media (min-width: ${({ theme: { mediaQueries } }) => mediaQueries.mobileBreakpoint}) {
    & > *:not(:first-child):not(:last-child) {
      margin-right: 16px;
    }
  }
`;

const Item = styled(NavLink)`
  color: white;
  font-size: 14px;
  text-decoration: none;
  text-transform: capitalize;

  &.active {
    font-family: neue-bold;
  }

  &:hover {
    color: white;
  }
`;

const Header = () => {
  const history = useHistory();

  const [showEthModal, setShowEthModal] = useState(false);

  return (
    <Container>
      <LeftContainer>
        <KDALogo style={{ cursor: 'pointer' }} onClick={() => history.push(ROUTE_INDEX)} />
        <Item to={ROUTE_SWAP}>swap</Item>
        <Item to={ROUTE_POOL}>pool</Item>
        <Item to={ROUTE_WRAP}>wrap</Item>
      </LeftContainer>
      <RightContainer>
        <Item className="mobile-none" to="#">
            {reduceToken('0x34161278361278361287360FA2')}
          </Item>
          <Item className="mobile-none" to="#">
              {reduceToken('kda-123')}
            </Item>
        <Modal
          trigger={<Button>ETH Wallet</Button>}
          content={<EthModal/>}
          actions={[{ key: 'done', content: 'Done', positive: true }]}
        />
        <Modal
          trigger={<Button>KDA Wallet</Button>}
          content={<KdaModal/>}
          actions={[{ key: 'done', content: 'Done', positive: true }]}
        />
        <Item to="#">
          <CogIcon />
        </Item>
        <Item to="#">
          <HamburgerIcon />
        </Item>
      </RightContainer>
    </Container>
  );
};

export default Header;
