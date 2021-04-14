import React from 'react';
import PropTypes from 'prop-types';
import styled from 'styled-components/macro';
import { ReactComponent as ArrowDown } from '../../assets/images/shared/arrow-down.svg';

const Container = styled.div`
  cursor: pointer;
  position: absolute;
  top: 30%;
  right: 8px;
  display: flex;
  justify-content: space-between;
  align-items: center;
  min-width: ${({ theme: { inputTokenWidth } }) => `${inputTokenWidth}px`};

  svg:first-child {
    margin-right: 8px;
  }

  span {
    font-size: 16px;
    margin-right: 13px;
  }
`;

const InputToken = ({ icon, code, onClick }) => {
  return (
    <Container onClick={onClick}>

        {icon}

      <span>{code}</span>
      <ArrowDown />
    </Container>
  );
};

InputToken.propTypes = {
  icon: PropTypes.element,
  code: PropTypes.string
};

InputToken.defaultProps = {
  icon: null,
  code: ''
};

export default InputToken;
