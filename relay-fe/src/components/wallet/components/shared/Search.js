import React from 'react';
import styled from 'styled-components/macro';
import { Search as SUISearch } from 'semantic-ui-react';

const Container = styled.div.attrs({ icon: 'search' })`
  .ui.search .prompt {
    border-radius: 4px;
  }
  .ui.input {
    width: ${({ fluid }) => (fluid ? '100%' : 'auto')};
  }
`;

const Search = ({ fluid, containerStyle, placeholder, value, onChange }) => {
  return (
    <Container fluid={fluid} style={containerStyle}>
      <SUISearch fluid open={false} icon="search" placeholder={placeholder} value={value} onSearchChange={onChange} />
    </Container>
  );
};

export default Search;
