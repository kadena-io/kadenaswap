import React, { useContext, useState } from 'react';
import { useHistory } from 'react-router-dom';
import { Message, Popup } from 'semantic-ui-react'
import Button from '../../shared/Button';
import styled from 'styled-components/macro';
import reduceToken from '../../../utils/reduceToken';
import {reduceBalance} from '../../../utils/reduceBalance';
import { WalletContext } from '../../../contexts/WalletContext';
import KdaModal from '../../../modals/kdaModal/KdaModal';
import { ReactComponent as KDALogo } from '../../../assets/images/header/kadena-logo.svg';
import { ReactComponent as PowerIcon } from '../../../assets/images/header/power.svg';
import { ReactComponent as CogIcon } from '../../../assets/images/header/cog.svg';
import { ReactComponent as HamburgerIcon } from '../../../assets/images/header/hamburger.svg';
import { ReactComponent as AboutIcon } from '../../../assets/images/header/about.svg';
import { ReactComponent as CodeIcon } from '../../../assets/images/header/code.svg';
import { ReactComponent as DiscordIcon } from '../../../assets/images/header/discord.svg';
import Input from '../../shared/Input';
import AccountInfo from '../header/AccountInfo';
import theme from '../../../styles/theme';

const Container = styled.div`
  position: fixed;
  top: 5px;
  left: 18px;
  display: flex;
  justify-content: space-between;
  /* min-height: ${({ theme: { header } }) => `${header.height}px`}; */
  width: calc(100% - 3em);
  /* @media (min-width: ${({ theme: { mediaQueries } }) => mediaQueries.mobileBreakpoint}) {
    width: inherit;
    left: unset;
  } */
`;

const LeftContainer = styled.div`
  display: flex;
  align-items: center;

  & > *:not(:last-child) {
    margin-right: 25px;
  }
`;

const Label = styled.span`
  font-size: 13px;
  text-transform: capitalize;
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
  /* @media (min-width: ${({ theme: { mediaQueries } }) => mediaQueries.mobileBreakpoint}) {
    & > *:not(:first-child):not(:last-child) {
      margin-right: 16px;
    }
  } */
`;

const Item = styled.div`
  color: white;
  font-size: 14px;
  text-decoration: none;
  text-transform: capitalize;
  &.active {
    font-family: neue-bold;
  }
  &:hover {
    color: white;
    opacity: 0.7;
    cursor: pointer;
  }
`;

const HamburgerListContainer = styled.div`
  border-radius: 4px;
`;

const HamburgerItem = styled.div`
  display: flex;
  align-items: center;
  font-family: neue-bold;
  font-size: 16px;
  color: ${({ theme: { colors } }) => colors.primary};
  &:hover {
    color: #000000;
    & svg {
      margin-right: 10px;
      & path {
        fill: #000000;
      }
    }
  }
  & svg {
    margin-right: 10px;
  }
`;

const Header = () => {

  const [showPactModal, setShowPactModal] = useState(false);
  const [openKdaModal, setOpenKdaModal] = useState(false);
  const history = useHistory();
  const pact = useContext(WalletContext);

  return (
      <Container>
        <LeftContainer>
        </LeftContainer>
        <RightContainer>
          <Item className="mobile-none" to="#">
          <Label
            style={{ padding: '10px 16px', color:"white", fontSize: 16 }}
          >
            LIVE {pact.NETWORK_ID === "testnet04" ? "TESTNET" : ""} ON CHAIN {pact.CHAIN_ID}
          </Label>
          </Item>
          {pact?.account.account ? (
            <AccountInfo
              onClick={() => setOpenKdaModal(true)}
              account={pact?.account.account ? `${reduceToken(pact?.account.account)}` : 'KDA'}
              balance={pact?.account.account ? `${reduceBalance(pact?.account.balance)} KDA` : ''}
            ></AccountInfo>
          ) : (
            <></>
          )}
          {!pact?.account.account && (
            <>
              <Item className="mobile-none" to="#">
                <Button
                  hover={true}
                  background="white"
                  color={theme.colors.pink}
                  buttonStyle={{ padding: '10px 16px' }}
                  fontSize={14}
                  onClick={() => setOpenKdaModal(true)}
                >
                  Connect Wallet
                </Button>
              </Item>
            </>
          )}
          {pact?.account.account && (
            <Item to="#" onClick={pact.logout}>
              <PowerIcon></PowerIcon>
            </Item>
          )}
        </RightContainer>
        <KdaModal open={openKdaModal} onClose={() => setOpenKdaModal(false)} />
      </Container>
  );
};

export default Header;
