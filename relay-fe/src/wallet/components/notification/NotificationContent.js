import React from 'react';
import styled from 'styled-components/macro';

const Container = styled.div`
  display: flex;
  align-items: center;
  padding: 8px;
`;

const TextContainer = styled.div`
  display: flex;
  flex-direction: column;
`;

const Text = styled.span`
  margin-left: 16px;
  margin-bottom: ${({ marginBottom }) => `${marginBottom}px` || '0px'};
  font-size: 16px;
  font-family: ${({ fontWeight }) => (fontWeight ? 'neue-bold' : 'neue-regular')};
`;

const capitalizeFirstLetter = (string) => {
  return typeof string === 'string' ? string.charAt(0).toUpperCase() + string.slice(1) : null;
};

const NotificationContent = ({ icon, type, message, title }) => {
  return (
    <Container>
      {icon}
      <TextContainer>
        <Text marginBottom={8} fontWeight="bold">
          {capitalizeFirstLetter(title) || capitalizeFirstLetter(type)}
        </Text>
        <Text>{capitalizeFirstLetter(message)}</Text>
      </TextContainer>
    </Container>
  );
};

export default NotificationContent;
