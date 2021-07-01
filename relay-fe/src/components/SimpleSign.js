import React, { useState, useContext, useEffect } from 'react';
import '../App.css';
import { Modal, Button, Form, Message, Icon, List, Input, Label } from 'semantic-ui-react';
import { PactContext } from "../contexts/PactContext";
import { WalletContext } from "../wallet/contexts/WalletContext"
import MainnetHome from "./MainnetHome"
import Pact from 'pact-lang-api';
const SimpleSign = (props) => {
  const pact = useContext(PactContext);
  const wallet = useContext(WalletContext);
  const [open, setOpen] = React.useState(false);
  const [key, setKey] = React.useState(false);
  const [acct, setAcct] = React.useState("");
  const {activity, bond, bondExist} = props;

  return (
    <Modal
      open={open}
      onClose={() => setOpen(false)}
      onOpen={() => setOpen(true)}
      style={{width: "500px"}}
      trigger={
        <Button
          disabled={!bondExist}
          style={{
            backgroundColor: "#18A33C",
            color: "white",
            width: 170,
          }}
        >
        {activity}
        </Button>
      }
    >
      <Modal.Header>Sign with your Bond key</Modal.Header>
      <Modal.Content >
        <Modal.Description>
          <p>
            Bond: {bond}
          </p>
          {activity === "Unbond" ?
          <Input
            placeholder="Enter Bond Account Associated with the Bond"
            style={{width: "360px"}}
            onChange={(e) => setAcct(e.target.value)}
          />
          : ""}
          <Input
            placeholder="Enter Bond Private Key"
            style={{width: "360px"}}
            onChange={(e) => setKey(e.target.value)}
          />
        </Modal.Description>
      </Modal.Content>
      <Modal.Actions>
        {activity === "Unbond" ?
        <Button
          onClick={() => {
            setOpen(false)
            pact.unBond(wallet.account.account, bond, key)
          }}
          primary
          disabled={key.length!==64}
          >
          Unbond
        </Button>
        :
        <Button
          onClick={() => {
            setOpen(false)
            pact.renewBond(bond, key)
          }}
          primary
          disabled={key.length!==64}
          >
          Renew
        </Button>}
      </Modal.Actions>
    </Modal>
  )
}

export default SimpleSign
