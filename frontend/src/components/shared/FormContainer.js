import React from 'react';
import PropTypes from 'prop-types';
import styled from 'styled-components/macro';
import { ReactComponent as CloseIcon } from '../../assets/images/shared/cross.svg';

const Container = styled.div`
  position: relative;
  display: flex;
  flex-flow: column;
  padding: 20px 20px;
  max-width: 385px;
  width: 100%;
  border-radius: 10px;
  background-color: white;
`;

const Title = styled.span`
  font-family: neue-bold;
  font-size: 24px;
  margin-bottom: 24px;
  text-transform: capitalize;
  text-align: center;
`;

const FormContainer = ({ title, containerStyle, titleStyle, children, onClose }) => {
  return (
    <Container style={containerStyle}>
      {onClose && <CloseIcon style={{ cursor: 'pointer', position: 'absolute', top: 18, right: 14 }} onClick={onClose} />}
      {title && <Title style={titleStyle}>{title}</Title>}
      {children}
    </Container>
  );
};

FormContainer.propTypes = {
  title: PropTypes.string,
  onClose: PropTypes.func
};

FormContainer.defaultProps = {
  title: '',
  onClose: null
};

export default FormContainer;
