import React, { useContext, useState, useEffect } from 'react';
import styled from 'styled-components/macro';
import { Transition } from 'react-spring/renderprops';
import { Message, Input, Icon } from 'semantic-ui-react';
import FormContainer from './FormContainer';
import Search from './Search';
import Backdrop from './Backdrop';
import Button from './Button'
import { ReactComponent as SuccessfulIcon } from '../../assets/images/shared/successful-circle.svg';
import { ReactComponent as ErrorIcon } from '../../assets/images/shared/error-circle.svg';
import { WalletContext } from '../../contexts/WalletContext';

// const Container = styled.div`
//   position: absolute;
//   display: flex;
//   justify-content: center;
//   align-items: center;
//   max-width: 385px;
//   width: 100%;
//   z-index: 5;
// `;

const Label = styled.span`
  font-family: neue-bold;
  font-size: 13px;
  /* color: ${({ theme: { colors } }) => colors.primary}; */
`;

const RowContainer = styled.div`
  display: flex;
  justify-content: space-between;
  margin: 15px 0px;
`;

const Divider = styled.div`
  /* border: ${({ theme: { colors } }) => `1px solid ${colors.border}`}; */
  margin: 16px 0px;
  width: 100%;
`;

const Content = styled.div`
  display: flex;
  align-items: center;
  flex-direction: column;
`;

const Title = styled.div`
  font-family: neue-bold;
  font-size: 24px;
  padding: 16px;
  /* color: ${({ theme: { colors } }) => colors.primary}; */
`;

const SubTitle = styled.div`
  font-family: neue-bold;
  font-size: 16px;
  /* color: ${({ theme: { colors } }) => colors.primary}; */
`;

const TransactionsDetails = styled.div`
  width: 100%;
  display: flex;
  flex-direction: column;
  padding: 24px 0px;
`;

const SpaceBetweenRow = styled.div`
  display: flex;
  align-items: center;
  justify-content: space-between;
`;

const Value = styled.span`
  font-family: neue-regular;
  font-size: 13px;
  /* color: ${({ theme: { colors } }) => colors.primary}; */
`;

const ContainerInputTypeNumber = styled.div`
  /* display: flex; */
  align-items: center;
  padding: 6.5px 8.5px;
  border-radius: 5px;
  /* border: ${({ theme: { colors } }) => `1px solid ${colors.border}`}; */

  .ui.input > input {
    border: unset;
    padding: 0px;
    text-align: left;
    font-size: 14px;
  }
  .ui.fluid.input > input {
    width: 200px !important;
  }
`;


const PasswordPopup = ({}) => {
  const pact = useContext(WalletContext);

  const [loading, setLoading] = useState(false)
  const [pw, setPw] = useState("");

  const password = () => {
    return (
      <Content>
        <SubTitle>Enter Password To Decrypt Private Key</SubTitle>
        <TransactionsDetails>
          <Input
            icon='lock'
            iconPosition='left'
            placeholder={"Enter Your Password"}
            value={pw}
            type={'password'}
            onChange={(e, { value }) => {
              pact.setPw(value);
            }}
            disabled={pw === ""}
          />
          <Message size='tiny' color='purple' style={{textAlign: 'center'}}>
            <RowContainer>
              <div style={{ display: 'flex', flexFlow: 'column', alignItems: 'center' }}>
                <span style={{ fontFamily: 'neue-bold', fontSize: 16, marginBottom: 10 }}><Icon name='warning sign' /> Note</span>
                <span>
                  If you forgot your password you can always reset it using your private key in the account section
                </span>
              </div>
            </RowContainer>
          </Message>
        </TransactionsDetails>
        <Button
          onClick={() => {
            pact.setPwStatus("entered");
            // onClose()
          }}
        >
          Next
        </Button>
      </Content>
    )
  }

  // return (
  //         <Container>
  //           <FormContainer title="sign transaction" containerStyle={{ height: '100%', maxHeight: '80vh', maxWidth: '90vw' }}
  //           // onClose={onClose}
  //           >
  //             {password()}
  //           </FormContainer>
  //         </Container>
  // );
  return (
    <FormContainer title="sign transaction"
    //           // onClose={onClose}
              >
      <SubTitle>Enter Password To Decrypt Private Key</SubTitle>
      <TransactionsDetails>
        <Input
          icon='lock'
          iconPosition='left'
          placeholder={"Enter Your Password"}
          value={pw}
          type={'password'}
          onChange={(e, { value }) => {
            pact.setPw(value);
          }}
          disabled={pw === ""}
        />
        <Message size='tiny' color='purple' style={{textAlign: 'center'}}>
          <RowContainer>
            <div style={{ display: 'flex', flexFlow: 'column', alignItems: 'center' }}>
              <span style={{ fontFamily: 'neue-bold', fontSize: 16, marginBottom: 10 }}><Icon name='warning sign' /> Note</span>
              <span>
                If you forgot your password you can always reset it using your private key in the account section
              </span>
            </div>
          </RowContainer>
        </Message>
      </TransactionsDetails>
      <Button
        onClick={() => {
          pact.setPwStatus("entered");
          // onClose()
        }}
      >
        Next
      </Button>
    </FormContainer>
  );
};

export default PasswordPopup;
