# Pedestals in stacked orders

What the treatises say about pedestals and podiums when orders are stacked one above another.

A pedestal is optional on any storey. Pedestals should get lower as you go up, and an upper pedestal should never be taller than the one below. No rule requires pedestals on all storeys or on none.

## What the treatises say

- **Vitruvius** gives the clearest rule, for the stage front (*scaena*) of a Roman theatre (Book V, ch. 6):
  - The bottom storey stands on a podium a third of its columns' height (1/12 of the orchestra's diameter, against columns of 1/4).
  - The parapet under the second storey is half the height of the one below; a third storey's parapet is half again.
  - The columns themselves shrink by a quarter per storey.
- **Chambers**, "Of Orders above Orders":
  - Palladio and Scamozzi put the second order on a pedestal.
  - With two storeys of **arcades** a pedestal "cannot be avoided", because without one the upper arches can't get a decent proportion.
  - In **colonnades** (columns carrying straight beams, no arches) the pedestal "may and ought" to be left out. It makes the upper storey "too predominant", and its projecting base looks like "much too heavy a load" on the cornice below.
  - He cites Palladio's Palazzo Barbarano, where the second storey stands "on a plinth only" (a plain low block).
- **Chambers**, "Of Pedestals", on ancient practice:
  - Lower pedestals were tall wherever crowds stood around, as in theatres, triumphal arches and temples, so the columns stayed visible over people's heads.
  - Upper pedestals were "very low". In amphitheatres they doubled as the railing in the arch openings, so they were only as high as someone needs to lean on.
  - Otherwise their only job was to raise the columns "so as to prevent any part of them from being hid by the projection of the cornice below".
  - In houses, the upper pedestal's height is "generally determined by the cills of the windows".
- **Palladio** notes that ancient temples have no pedestals at all, which "pleases me much better".

## Rules for a generative algorithm

1. **No pedestals anywhere** is always correct.
2. **Bottom storey only**, as pedestals or a podium, is the most common choice. It is needed when the columns must rise above people, steps or sloping ground. A rusticated basement storey often does this job instead.
3. **Upper storeys:** a low continuous parapet or plinth is fine, just high enough to clear the cornice below or to serve as a balustrade or window-sill line. Over arcades it is required. A full ⅓-height pedestal on an upper colonnade storey is what Chambers objects to.
4. **Never** make an upper pedestal taller than the one below it.
5. **Individual pedestals vs. a podium:** upper storeys usually get the continuous form (parapet, balustrade). Separate pedestals under each column mostly belong to the bottom storey or to arcades.

## Implications for this project

The collonade pages draw every storey's pedestal at the author's full height. Turning one on in an upper row therefore produces the tall upper pedestal that Chambers warns against. Following the rules above would need either a low pedestal mode for upper storeys (sized by Vitruvius' halving, or just enough to clear the cornice below) or disabling pedestals above a storey that has none.

## Sources

- [Vitruvius, *The Ten Books on Architecture*, Morgan translation](https://www.gutenberg.org/ebooks/20239)
- [Chambers, *A Treatise on the Decorative Part of Civil Architecture*](https://archive.org/details/treatiseondecora00chamuoft)
- [Palladio, *The Four Books of Architecture*, Ware translation, 1738](https://archive.org/stream/gri_33125011569684/gri_33125011569684_djvu.txt)
