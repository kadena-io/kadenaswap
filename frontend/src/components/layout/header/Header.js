import React, { useContext, useState } from 'react';
import { NavLink, useHistory } from 'react-router-dom';
import { Modal, Message, Popup } from 'semantic-ui-react'
import Button from '../../shared/Button';
import styled from 'styled-components/macro';
import reduceToken from '../../../utils/reduceToken';
import reduceBalance from '../../../utils/reduceBalance';
import { PactContext } from '../../../contexts/PactContext';
import KdaModal from '../../../modals/kdaModal/KdaModal';
import { ROUTE_INDEX, ROUTE_POOL, ROUTE_SWAP, ROUTE_WRAP } from '../../../router/routes';
import { ReactComponent as KDALogo } from '../../../assets/images/header/kadena-logo.svg';
import { ReactComponent as PowerIcon } from '../../../assets/images/header/power.svg';
import { ReactComponent as CogIcon } from '../../../assets/images/header/cog.svg';
import { ReactComponent as HamburgerIcon } from '../../../assets/images/header/hamburger.svg';
import Input from '../../shared/Input';

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

const Label = styled.span`
  font-size: 13px;
  font-family: neue-bold;
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

  const [showEthModal, setShowEthModal] = useState(false);
  const [showPactModal, setShowPactModal] = useState(false);
  const history = useHistory();
  const pact = useContext(PactContext);

  return (
    <Container>
      <LeftContainer>
        <KDALogo style={{ cursor: 'pointer' }} onClick={() => history.push(ROUTE_INDEX)} />
        <Item to={ROUTE_SWAP}>swap</Item>
        <Item to={ROUTE_POOL}>pool</Item>
        <Item to={ROUTE_WRAP}>wrap</Item>
      </LeftContainer>
      <RightContainer>
      {(pact.polling
        ?
          <Item className="mobile-none" to="#">
            <Message color='yellow' size='mini'>
              {`tx ${reduceToken(pact.localRes.reqKey)} pending...`}
            </Message>
          </Item>
        :
          <></>
      )}
      {(pact.sendRes
        ?
          <Item className="mobile-none" to="#">
            <Message color={(pact.sendRes.result.status === 'success' ? 'green' : 'red')} size='mini'>
              <span
                onClick={async () => {
                  await window.open(
                    `https://explorer.chainweb.com/testnet/tx/${pact.sendRes.reqKey}`,
                    "_blank",
                    'noopener,noreferrer'
                  );
                  pact.clearSendRes();
                }}
              >
                {`view tx ${(pact.sendRes.result.status === 'success' ? 'success' : 'failure')}`}
              </span>
            </Message>
          </Item>
        :
          <></>

      )}
      {(pact.account.account
        ?
        <>
          <Item className="mobile-none" to="#">
            <Message color='violet' size='mini'>
              {pact.account.account ? `${reduceToken(pact.account.account)}`: "KDA"}
            </Message>
          </Item>
          <Item className="mobile-none" to="#">
              <Message color='purple' size='mini'>
                {pact.account.account ? `${reduceBalance(pact.account.balance)} KDA`: ""}
              </Message>
          </Item>
        </>
        :
        <></>
      )}

        {/*
        <Modal
          trigger={<Button>ETH Wallet</Button>}
          // content={<EthModal/>}
          actions={[{ key: 'done', content: 'Done', positive: true }]}
        />
        */}
        <Item className="mobile-none" to="#">

          <KdaModal
            // trigger={<Button>Wallet</Button>}
          />
        </Item>
        <Item to="#">
          <Popup
           trigger={<CogIcon />}
           on='click'
           offset={[0, 10]}
           position='bottom center'
          >
            <Label style={{ margin: 30 }}>Slippage (%)</Label>
            <Input
              error={pact.account.account === null}
              value={(isNaN(pact.slippage) ? ' ' : `${pact.slippage*100}`)}
              onChange={async (e, { value }) => {
                pact.storeSlippage(value/100);
              }}
            />
          </Popup>
        </Item>
        <Item to="#">
          <HamburgerIcon />
        </Item>
      </RightContainer>
    </Container>
  );
};

export default Header;
