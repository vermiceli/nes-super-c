-- Name: irqCallback
-- Author: Johnybot
-- Draws a horizontal line on the scanline that an IRQ is called

function irqCallback()
  scanline = emu.getState()["ppu.scanline"]
  emu.drawLine(0, scanline + 1, 256, scanline + 1, 0x44E53935)
end

emu.addEventCallback(irqCallback, emu.eventType.irq)