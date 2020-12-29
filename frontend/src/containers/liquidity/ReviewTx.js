import React, { useEffect, useContext, useState } from 'react';
import { Header, Modal } from 'semantic-ui-react'
import Button from '../../components/shared/Button';
import Input from '../../components/shared/Input';
import { PactContext } from '../../contexts/PactContext'
import { Statistic, List, Divider } from 'semantic-ui-react'
import cryptoCurrencies from '../../constants/cryptoCurrencies';
import reduceBalance from '../../utils/reduceBalance';
import TxView from '../../components/shared/TxView';
import { ReactComponent as KadenaLogo } from '../../assets/images/crypto/kadena-logo.svg';

function ReviewTx(props) {
  const pact = useContext(PactContext);
  const {fromValues, toValues, buttonStatus, liquidityView, loading, supply, open, setOpen} = props;
  return (
      <Modal
        onClose={() => setOpen(false)}
        onOpen={() => setOpen(true)}
        open={open}
        style={{
          width:327
        }}
        trigger={
          <Button
            disabled={!buttonStatus().status}
            buttonStyle={{ marginTop: 24, marginRight: 0 }}>
            {buttonStatus().msg}
          </Button>}
      >
        <Modal.Content image>

        <Modal.Description>
          <Header>Review Details</Header>
          <Statistic>
            <Statistic.Label>{`${fromValues.coin} / ${toValues.coin} Pool Tokens`}</Statistic.Label>
          </Statistic>
          <Divider/>
          {liquidityView === "Add Liquidity"
          ?
          <List>
            <List.Item>{`${fromValues.coin} Deposit Desired: ${reduceBalance(fromValues.amount)}`}</List.Item>
            <List.Item>{`${toValues.coin} Deposit Desired: ${reduceBalance(toValues.amount)}`}</List.Item>
            <br/>
            <List.Item>{`Rates:`}</List.Item>
            <br/>
            <List.Item>{`1 ${fromValues.coin} = ${reduceBalance(1/pact.ratio)} ${toValues.coin}`}</List.Item>
            <List.Item>{`1 ${toValues.coin} = ${reduceBalance(pact.ratio)} ${fromValues.coin}`}</List.Item>
            <List.Item>Share of the Pool : {reduceBalance(pact.share(fromValues.amount)*100)}%</List.Item>
          </List>
          :
          <List>
            <List.Item>{`${fromValues.coin} Deposit Desired: ${reduceBalance(fromValues.amount)}`}</List.Item>
            <List.Item>{`${toValues.coin} Deposit Desired: ${reduceBalance(toValues.amount)}`}</List.Item>
            <br/>
            <List.Item>{`Rates:`}</List.Item>
            <br/>
            <List.Item>{`1 ${fromValues.coin} = ${reduceBalance(toValues.amount/fromValues.amount)} ${toValues.coin}`}</List.Item>
            <List.Item>{`1 ${toValues.coin} = ${reduceBalance(fromValues.amount/toValues.amount)} ${fromValues.coin}`}</List.Item>
          </List>

          }
        </Modal.Description>
        </Modal.Content>
        <Modal.Actions>
          <Button
            loading={loading}
            onClick={supply}>
            Confirm
          </Button>
        </Modal.Actions>
      </Modal>
  )
}

export default ReviewTx;
