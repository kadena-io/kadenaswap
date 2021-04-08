import React from 'react'
import { Button, Header, Image, Modal, Label, Divider } from 'semantic-ui-react'
import { NavLink, useHistory } from 'react-router-dom';
import { ROUTE_INDEX, ROUTE_POOL, ROUTE_SWAP, ROUTE_KPY_RED } from '../router/routes';

function ModalExampleModal() {
  const [open, setOpen] = React.useState(true)

  return (
    <Modal
      onClose={() => setOpen(false)}
      onOpen={() => setOpen(true)}
      open={open}
    >
      <Modal.Header>
        <p>Quick Guide on KPenny Redeem  <Label as='a' color='red' tag> Upcoming </Label></p>
      </Modal.Header>
      <Modal.Content>
        <h4> To do BEFORE 4/15/2021 00:00 UTC</h4>
        <Modal.Description>
          <p>
            1. Remove your liquidity pair (ABC/XYZ, ABC/KPY, KPY/XYZ) from the <NavLink to={ROUTE_POOL} onClick={() => setOpen(false)}>liquidity pool</NavLink>.
          </p>
          <p>
            2. Swap your ABC, and XYZ tokens to KPY <NavLink to={ROUTE_SWAP} onClick={() => setOpen(false)}> here</NavLink>!
          </p>
          <Divider/>
          <h4> To do AFTER 4/15/2021 00:00 UTC </h4>
          <p>
            3. Redeem your KPY to KDA <NavLink to={ROUTE_KPY_RED} onClick={() => setOpen(false)}> here</NavLink>!
          </p>
        </Modal.Description>
      </Modal.Content>
      <Modal.Actions>
        <Button color='teal' onClick={() => setOpen(false)}>
          Got it
        </Button>
      </Modal.Actions>
    </Modal>
  )
}

export default ModalExampleModal
