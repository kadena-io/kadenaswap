import React, { useContext, useState } from 'react';
import { Header, Modal, Menu, Icon } from 'semantic-ui-react'
import Input from '../../components/shared/Input';
import Button from '../../components/shared/Button';
import { PactContext } from '../../contexts/PactContext'

export default function Account() {
  const pact = useContext(PactContext);
  const [fromInput, setInputValue] = useState({ account: pact.account.account });
  const [toggle, setToggle] = useState(false)
  const [pk, setPk] = useState("")
  const [pw, setPw] = useState("")
  const [pwConf, setPwConf] = useState("")
  const [open, setOpen] = React.useState(false)

  const is_hexadecimal = (str) => {
     const regexp = /^[0-9a-fA-F]+$/;
     if (regexp.test(str)) return true;
     else return false;
  }

  const checkKey = (key) => {
     if (key.length !== 64) {
         return false
     } else if (!is_hexadecimal(key)){
         return false
     }
     return true;
  }

  return (
    <>
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
          error={pact.account.account === null}
          value={fromInput.account}
          onChange={async (e, { value }) => {
            setInputValue(value);
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
            <Header style={{ color: 'red' }}>{"Account Does Not Exist"}</Header>
        )}
        <Header>{"Signing Method"}</Header>
        <Menu color="purple" widths={3} >
          <Menu.Item
            name='pk'
            active={pact.signing.method === 'pk'}
            onClick={() => pact.setSigningMethod('pk')}
            disabled={!toggle}
          >
            <Icon name='warning sign' />
            Plain Private Key (unsafe)
          </Menu.Item>

          <Menu.Item
            name='pk+pw'
            active={pact.signing.method === 'pk+pw'}
            onClick={() => pact.setSigningMethod('pk+pw')}
            disabled={!toggle}
          >
            <Icon name='lock' />
            Private Key + Password (safe)
          </Menu.Item>

          <Menu.Item
            name='sign'
            active={pact.signing.method === 'sign'}
            onClick={() => pact.setSigningMethod('sign')}
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
            (pact.signing.method === 'pk'
              ?
                <>
                <Header>{"Your Private Key"}</Header>
                <Input
                  value={pact.signing.key}
                  onChange={(e, { value }) => pact.storePrivKey(value)}
                  // error={!checkKey(pact.signing.key)}
                />
                </>
              :
                (pact.signing.method === 'pk+pw'
                  ?
                    <>
                    <Header>{"Your Private Key"}</Header>
                    <Input
                      value={pk}
                      onChange={(e, { value }) => setPk(value)}
                      error={!checkKey(pk)}
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
                    <></>
                )
            )
        )}
        <Header>{"[TESTING] Your Private Key"}</Header>
        <Input
          value={pact.privKey}
          onChange={(e, { value }) => pact.storePrivKey(value)}
          type='password'
          error={!checkKey(pact.privKey)}
        />
      </Modal.Description>
      </Modal.Content>
      {/*
      <Modal.Actions>
        <Button
          onClick={async () => {
            try{
              await pact.setVerifiedAccount(fromInput.account);
            } catch (e){
              console.log(e)
            }
          }}
        >Connect</Button>
      </Modal.Actions>
      */}
    </>
  )
}
