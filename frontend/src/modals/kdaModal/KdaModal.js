import React, { useContext, useState } from 'react';
import { Header, Modal, Menu, Icon, Message } from 'semantic-ui-react'
import Input from '../../components/shared/Input';
import Button from '../../components/shared/Button';
import { PactContext } from '../../contexts/PactContext'

export default function Account(props) {

  const pact = useContext(PactContext);

  const [fromInput, setInputValue] = useState({ account: pact.account.account });
  const [toggle, setToggle] = useState(false)
  const [method, setMethod] = useState(pact.signing.method)
  const [pk, setPk] = useState("")
  const [pw, setPw] = useState("")
  const [pwConf, setPwConf] = useState("")
  const [open, setOpen] = React.useState(false)
  const [temp, setTemp] = useState("")

  console.log('acct', fromInput.account)

  const is_hexadecimal = (str) => {
     const regexp = /^[0-9a-fA-F]+$/;
     if (regexp.test(str)) return true;
     else return false;
  }

  const checkKey = (key) => {
    try {
      if (key.length !== 64) {
          return false
      } else if (!is_hexadecimal(key)){
          return false
      }
      return true;
    } catch (e) {
      console.log(e);
      return false
    }
  }

  const canSubmit = () => {
    if (method === 'sign') return true
    if (method === 'pk' && checkKey(pk)) return true
    if (method === 'pk+pw' && pw === pwConf && checkKey(pk) && pw !== "") return true
    return false
  }

  const resetValues = () => {
    setToggle(false);
    setPk("");
    setPw("");
    setPwConf("")
  }

  return (
    <Modal
      trigger={
        <Button
          buttonStyle={props.buttonStyle}
        >
          {props.buttonName? props.buttonName : "Wallet"}
        </Button>}
      onClose={() => {
        pact.setRegistered(true);
        resetValues()
        setOpen(false)
      }}
      onOpen={() => setOpen(true)}
      open={open}
      closeIcon
    >
    <Modal.Content image>
      <Modal.Description>
        <Header>
          <span style={{ marginRight: 20 }} >
            {"Your KDA Account"}
          </span>
          {(toggle
            ?
              <></>
            :
                <Button
                  onClick={() => setToggle((toggle ? false : true))}
                >
                  <Icon name='unlock alternate' />
                  unlock
                </Button>
          )}
        </Header>
        <Input
          error={pact.account.account === null && temp !== ""}
          value={fromInput.account}
          onChange={async (e, { value }) => {
            setInputValue(value);
            setTemp(value)
            await pact.setVerifiedAccount(value);
          }}
        />
        {(pact.account.account
          ?
            <>
              <Header>{"Account Details"}</Header>
              <span>{JSON.stringify(pact.account.guard)}</span>
            </>
          :
            (temp === "" ? <></> : <Header style={{ color: 'red' }}>{"Account Does Not Exist"}</Header>)

        )}
        <Header>{"Signing Method"}</Header>
        <Menu color="purple" widths={3} >
          <Menu.Item
            name='pk'
            active={method === 'pk'}
            onClick={() => setMethod('pk')}
            disabled={!toggle}
          >
            <Icon name='warning sign' />
            Plain Private Key (unsafe)
          </Menu.Item>

          <Menu.Item
            name='pk+pw'
            active={method === 'pk+pw'}
            onClick={() => setMethod('pk+pw')}
            disabled={!toggle}
          >
            <Icon name='lock' />
            Private Key + Password (safe)
          </Menu.Item>

          <Menu.Item
            name='sign'
            active={method === 'sign'}
            onClick={() => setMethod('sign')}
            disabled={!toggle}
          >
            <Icon name='signup' />
            Chainweaver Signing (safest)
          </Menu.Item>

        </Menu>
        {(!toggle
          ?
            <></>
          :
            (method === 'pk'
              ?
                <>
                  <Message negative>
                    <Header><Icon name='warning' style={{ margin: 10 }}/>{"NOTE"}</Header>
                    <div style={{ margin: 10, marginBottom: 20 }}>
                      <p>
                        All your transactions will be automatically signed with these keys
                      </p>
                      <p>
                        Your private key will be saved in browser storage making easily accessible to malicious actors
                      </p>
                    </div>
                  </Message>
                  <Header>{"Your Private Key"}</Header>
                  <Input
                    value={pk}
                    onChange={(e, { value }) => setPk(value)}
                    error={(pk !== "" ? !checkKey(pk) : false)}
                  />
                </>
              :
                (method === 'pk+pw'
                  ?
                    <>
                      <Message color='yellow'>
                        <Header><Icon name='warning' style={{ margin: 10 }}/>{"NOTE"}</Header>
                        <div style={{ margin: 10, marginBottom: 20 }}>
                          <p>
                            You will be prompted to enter your password to submit transactions
                          </p>
                          <p>
                            You can always reset your password by following this process again with your private key
                          </p>
                        </div>
                      </Message>
                      <Header>{"Your Private Key"}</Header>
                      <Input
                        value={pk}
                        onChange={(e, { value }) => setPk(value)}
                        error={(pk !== "" ? !checkKey(pk) : false)}
                      />
                      <Header>{"Your Password"}</Header>
                      <Input
                        value={pw}
                        onChange={(e, { value }) => setPw(value)}
                        type='password'
                        error={pw !== pwConf}
                      />
                      <Header>{"Confirm Password"}</Header>
                      <Input
                        value={pwConf}
                        onChange={(e, { value }) => setPwConf(value)}
                        type='password'
                        error={pw !== pwConf}
                      />

                    </>
                  :
                    <>
                      <Message positive>
                        <Header><Icon name='warning' style={{ margin: 10 }}/>{"NOTE"}</Header>
                        <div style={{ margin: 10, marginBottom: 20 }}>
                          <p>
                            Please make sure the KDA account provided is controlled by your Chainweaver wallet
                          </p>
                          <p>
                            When submitting a transaction, Chainweaver will show you a preview within the wallet before signing
                          </p>
                          <p
                            onClick={async () => {
                              await window.open(
                                `https://www.kadena.io/chainweaver`,
                                "_blank",
                                'noopener,noreferrer'
                              );
                            }}
                          >
                            Download Chainweaver <a>here</a>
                          </p>
                        </div>
                      </Message>
                    </>
                )
            )
        )}
        {/*
        <Header>{"[TESTING] Your Private Key"}</Header>
        <Input
          value={pact.privKey}
          onChange={(e, { value }) => pact.storePrivKey(value)}
          type='password'
          error={!checkKey(pact.privKey)}
        />
        */}
      </Modal.Description>

      </Modal.Content>
      <Modal.Actions>
        <Button
          onClick={() => {
            resetValues()
            setOpen(false)
          }}
        >
          <Icon name='cancel' />
          cancel
        </Button>
        {(!toggle
          ?
            <></>
          :
            <>
              <Button
                onClick={async () => {
                  if (method === 'pk') await pact.storePrivKey(pk)
                  if (method === 'pk+pw') await pact.encryptKey(pk, pw)
                  if (method === 'sign') await pact.signingWallet()
                  resetValues();
                  setOpen(false)
                }}
                disabled={!canSubmit()}
              >
                <Icon name='lock' />
                update
              </Button>
            </>
        )}
      </Modal.Actions>
    </Modal>
  )
}
