import React from 'react'
import { Button, Header, Image, Modal, Label, Divider } from 'semantic-ui-react'

function ModalExampleModal() {
  const [open, setOpen] = React.useState(true)

  return (
    <Modal
      onClose={() => setOpen(false)}
      onOpen={() => setOpen(true)}
      open={open}
    >
      <Modal.Header>
        <p>Quick Guide to Redeeming KPenny <Label as='a' color='red' tag> Upcoming </Label></p>
      </Modal.Header>
      <Modal.Content>
        <h4> To do BEFORE 4/15/2021 00:00 UTC</h4>
        <Modal.Description>
          <p>
            1. Remove your liquidity pair (ABC/XYZ, ABC/KPY, KPY/XYZ) from the <a href="https://kadenaswap.chainweb.com/pool">liquidity pool</a>.
          </p>
          <p>
            2. Swap your ABC, and XYZ tokens to KPY <a href="https://kadenaswap.chainweb.com/swap"> here</a>!
          </p>
          <Divider/>
          <h4> To do AFTER 4/15/2021 00:00 UTC </h4>
          <p>
            3. Redeem your KPY to KDA <a href="https://kadenaswap.chainweb.com/kpenny-redeem"> here</a>!
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
