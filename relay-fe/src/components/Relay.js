import React, { useState, useContext, useEffect } from 'react';
import '../App.css';
import { Form, Message, Icon, List, Input, Label } from 'semantic-ui-react';
import Button from '../wallet/components/shared/Button';
import { PactContext } from "../contexts/PactContext";
import { WalletContext } from "../wallet/contexts/WalletContext"
import Pact from 'pact-lang-api';

function Relay() {
  const pact = useContext(PactContext);
  const wallet = useContext(WalletContext);
  const [hash, setHash] = React.useState("")

    return (
          <Form inverted hidden>
            <Form.Field
              style={{marginTop: "10px", marginBottom: 10, width: "360px", marginLeft: "auto", marginRight: "auto"}}
              >
              <label style={{color: "#18A33C", textAlign: "left" }}>
                Propose or Endorse a header
              </label>
              <Input
                onChange={(e) => setHash(e.target.value)}
                placeholder='Eth Header'
                />
            </Form.Field>
           <Button
             style={{
               backgroundColor: "#18A33C",
               color: "white",
               width: 360,
             }}
           >Propose / Endorse</Button>
          </Form>
    );

}

export default Relay;
