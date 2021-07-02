import React, { useState, useContext, useEffect } from 'react';
import '../App.css';
import { Modal, Form, Message, Icon, List, Input, Label } from 'semantic-ui-react';
import Button from '../wallet/components/shared/Button';
import { PactContext } from "../contexts/PactContext";
import { WalletContext } from "../wallet/contexts/WalletContext"
import Pact from 'pact-lang-api';
const SimpleSign = (props) => {
  const pact = useContext(PactContext);
  const wallet = useContext(WalletContext);

  const [firstOpen, setFirstOpen] = React.useState(false)
  const [secondOpen, setSecondOpen] = React.useState(false)
  const [key, setKey] = React.useState(false);
  const {activity, bond, bondExist} = props;

  const BondInfo = () => {
    return(
      <List>
        <List.Item>
          <List.Header>Pool</List.Header>
          <List.Description>{pact.bondInfo.pool} </List.Description>
        </List.Item>
        <List.Item>
          <List.Header>Associated KDA Account</List.Header>
          <List.Description>{pact.bondInfo.account} </List.Description>
        </List.Item>
        <List.Item>
          <List.Header>Bond Balance</List.Header>
          <List.Description>{pact.bondInfo.balance} </List.Description>
        </List.Item>
        <List.Item>
          <List.Header>Bond Date</List.Header>
          <List.Description>{pact.bondInfo.date && pact.bondInfo.date.timep} </List.Description>
        </List.Item>
        <List.Item>
          <List.Header>Bond Activity</List.Header>
          <List.Description>{pact.bondInfo.activity && pact.bondInfo.activity.int} </List.Description>
        </List.Item>
      </List>
    )
  }

  return (
    //First Modal
    <Modal
        onClose={() => setFirstOpen(false)}
        onOpen={() => setFirstOpen(true)}
        open={firstOpen}
        style={{width: "750px", margin: 40}}
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
        <Modal.Header style={{textAlign:'center'}}>
        {activity}<br/>"{bond}"
        </Modal.Header>
        <Modal.Content >
          <Modal.Description>
            <BondInfo
            style={{marginLeft: "10px"}}/>
          </Modal.Description>
        </Modal.Content>
        <Modal.Actions style={{textAlign:'center'}}>
          {activity === "Unbond" ?
          <div >
            <Button
              onClick={() => {
                setFirstOpen(false)
                pact.unBond(pact.bondInfo.account, bond, key)
              }}
              primary
              >
              Sign with Chainweaver / Zelcore
            </Button>
            <Button
              onClick={() => {
                setSecondOpen(true)
                // setOpen(false)
              }}
              primary
              >
              Sign with Bond Private Key (unsafe)
            </Button>
          </div>
          :
          <div>
            <Button
              onClick={() => {
                setFirstOpen(false)
                pact.renewBond(bond, key)
              }}
              primary
              >
              Sign with Chainweaver / Zelcore
            </Button>
            <Button
              onClick={() => {
                setSecondOpen(true)
              }}
              primary
              >
              Sign with Private Key (unsafe)
            </Button>
          </div>
        }
        </Modal.Actions>

        <Modal
        onClose={() => setSecondOpen(false)}
        open={secondOpen}
        style={{width: "500px"}}
        >
        <Modal.Header>Sign with your Bond key</Modal.Header>
        <Modal.Content >
          <Modal.Description>
            <List>
              <List.Item>
                <List.Header>
                  Bond
                </List.Header>
                <List.Description>
                  {bond}
                </List.Description>
              </List.Item>
            </List>
            <p style={{
                color: "red"
              }}>
              Note: Pasting your private key is not safe. Do you want to continue?
            </p>
            <Input
              placeholder="Enter Bond Private Key"
              style={{width: "360px"}}
              onChange={(e) => setKey(e.target.value)}
            />
          </Modal.Description>
        </Modal.Content>
        <Modal.Actions>
          {activity === "Unbond" ?
          <div>
            <Button
              onClick={() => {
                setFirstOpen(false)
                setSecondOpen(false)
                pact.unBond(pact.bondInfo.account, bond, key, false)
              }}
              primary
              disabled={key.length!==64}
              >
              Unbond with Bond Private Key
            </Button>
          </div>
          :
          <div>
            <Button
              onClick={() => {
                setFirstOpen(false)
                setSecondOpen(false)
                pact.renewBond(bond, key, false)
              }}
              primary
              disabled={key.length!==64}
              >
              Renew with Private Key
            </Button>
          </div>
        }
        </Modal.Actions>
        </Modal>


    </Modal>

  )
}

export default SimpleSign
