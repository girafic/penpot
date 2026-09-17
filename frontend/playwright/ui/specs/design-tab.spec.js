import { test, expect } from "@playwright/test";
import { WasmWorkspacePage } from "../pages/WasmWorkspacePage";

const tokenInputFlag = "enable-feature-token-input";

test.beforeEach(async ({ page }) => {
  await WasmWorkspacePage.init(page);
  await WasmWorkspacePage.mockConfigFlags(page, [tokenInputFlag]);
});

const multipleConstraintsFileId = `03bff843-920f-81a1-8004-756365e1eb6a`;
const multipleConstraintsPageId = `03bff843-920f-81a1-8004-756365e1eb6b`;
const multipleAttributesFileId = `1795a568-0df0-8095-8004-7ba741f56be2`;
const multipleAttributesPageId = `1795a568-0df0-8095-8004-7ba741f56be3`;

const setupFileWithMultipeConstraints = async (workspace) => {
  await workspace.setupEmptyFile();
  await workspace.mockRPC(
    /get\-file\?/,
    "design/get-file-multiple-constraints.json",
  );
  await workspace.mockRPC(
    "get-file-object-thumbnails?file-id=*",
    "design/get-file-object-thumbnails-multiple-constraints.json",
  );
  await workspace.mockRPC(
    "get-file-fragment?file-id=*",
    "design/get-file-fragment-multiple-constraints.json",
  );
};

const setupFileWithMultipeAttributes = async (workspace) => {
  await workspace.setupEmptyFile();
  await workspace.mockRPC(
    /get\-file\?/,
    "design/get-file-multiple-attributes.json",
  );
  await workspace.mockRPC(
    "get-file-object-thumbnails?file-id=*",
    "design/get-file-object-thumbnails-multiple-attributes.json",
  );
};

test.describe("Constraints", () => {
  test("Constraint dropdown shows 'Mixed' when multiple layers are selected with different constraints", async ({
    page,
  }) => {
    const workspace = new WasmWorkspacePage(page);
    await setupFileWithMultipeConstraints(workspace);
    await workspace.goToWorkspace({
      fileId: multipleConstraintsFileId,
      pageId: multipleConstraintsPageId,
    });

    await workspace.clickToggableLayer("Board");
    await workspace.clickLeafLayer("Ellipse");
    await workspace.clickLeafLayer("Rectangle", { modifiers: ["Shift"] });

    const constraintVDropdown = workspace.page.getByTestId(
      "constraint-v-select",
    );
    await expect(constraintVDropdown).toContainText("Mixed");
    const constraintHDropdown = workspace.page.getByTestId(
      "constraint-h-select",
    );
    await expect(constraintHDropdown).toContainText("Mixed");

    expect(false);
  });
});

test.describe("Shape attributes", () => {
  test("Cannot add a new fill when the limit has been reached", async ({
    page,
  }) => {
    const workspace = new WasmWorkspacePage(page);
    await workspace.mockConfigFlags([
      "enable-feature-render-wasm",
      tokenInputFlag,
    ]);
    await workspace.setupEmptyFile();
    await workspace.mockRPC(/get\-file\?/, "design/get-file-fills-limit.json");

    await workspace.goToWorkspace({
      fileId: "d2847136-a651-80ac-8006-4202d9214aa7",
      pageId: "d2847136-a651-80ac-8006-4202d9214aa8",
    });

    await workspace.clickLeafLayer("Rectangle");

    await workspace.page.getByTestId("add-fill").click();
    await expect(
      workspace.page.getByRole("button", { name: "#B1B2B5" }),
    ).toHaveCount(8);

    await expect(workspace.page.getByTestId("add-fill")).toBeDisabled();
  });

  // FIXME: flaky
  test.skip("Cannot add a new text fill when the limit has been reached", async ({
    page,
  }) => {
    const workspace = new WasmWorkspacePage(page);
    await workspace.mockConfigFlags([
      "enable-feature-render-wasm",
      tokenInputFlag,
    ]);
    await workspace.setupEmptyFile();
    await workspace.mockRPC(
      /get\-file\?/,
      "design/get-file-text-fills-limit.json",
    );

    await workspace.goToWorkspace({
      fileId: "b1ff3fdf-b491-812b-8006-f2ce3d29333a",
      pageId: "b1ff3fdf-b491-812b-8006-f2ce3d29333b",
    });

    await workspace.clickLeafLayer("Lorem ipsum");

    await expect(
      workspace.page.getByRole("button", { name: "Remove color" }),
    ).toHaveCount(7);

    await workspace.page.getByRole("button", { name: "Add fill" }).click();
    await expect(
      workspace.page.getByRole("button", { name: "Remove color" }),
    ).toHaveCount(8);

    await expect(
      workspace.page.getByRole("button", { name: "Add fill" }),
    ).toBeDisabled();
  });
});

test.describe("Multiple shapes attributes", () => {
  test("User selects multiple shapes with sames fills, strokes, shadows and blur", async ({
    page,
  }) => {
    const workspace = new WasmWorkspacePage(page);
    await setupFileWithMultipeConstraints(workspace);
    await workspace.goToWorkspace({
      fileId: multipleConstraintsFileId,
      pageId: multipleConstraintsPageId,
    });

    await workspace.clickToggableLayer("Board");
    await workspace.clickLeafLayer("Ellipse");
    await workspace.clickLeafLayer("Rectangle", { modifiers: ["Shift"] });

    await expect(workspace.page.getByTestId("add-fill")).toBeVisible();
    await expect(workspace.page.getByTestId("add-stroke")).toBeVisible();
    await expect(workspace.page.getByTestId("add-shadow")).toBeVisible();
    await expect(workspace.page.getByTestId("add-blur")).toBeVisible();
  });

  test("User selects multiple shapes with different fills, strokes, shadows and blur", async ({
    page,
  }) => {
    const workspace = new WasmWorkspacePage(page);
    await setupFileWithMultipeAttributes(workspace);
    await workspace.goToWorkspace({
      fileId: multipleAttributesFileId,
      pageId: multipleAttributesPageId,
    });

    await workspace.clickLeafLayer("Ellipse");
    await workspace.clickLeafLayer("Rectangle", { modifiers: ["Shift"] });

    await expect(workspace.page.getByTestId("add-fill")).toBeHidden();
    await expect(workspace.page.getByTestId("add-stroke")).toBeHidden();
    await expect(workspace.page.getByTestId("add-shadow")).toBeHidden();
    await expect(workspace.page.getByTestId("add-blur")).toBeHidden();
  });
});

test("BUG 7760 - Layout losing properties when changing parents", async ({
  page,
}) => {
  const workspacePage = new WasmWorkspacePage(page);
  await workspacePage.setupEmptyFile();
  await workspacePage.mockRPC(/get\-file\?/, "workspace/get-file-7760.json");
  await workspacePage.mockRPC(
    "get-file-fragment?file-id=*&fragment-id=*",
    "workspace/get-file-fragment-7760.json",
  );
  await workspacePage.mockRPC(
    "update-file?id=*",
    "workspace/update-file-create-rect.json",
  );

  await workspacePage.goToWorkspace({
    fileId: "cd90e028-326a-80b4-8004-7cdec16ffad5",
    pageId: "cd90e028-326a-80b4-8004-7cdec16ffad6",
  });

  // Select the flex board and drag it into the other container board
  await workspacePage.clickLeafLayer("Flex Board");

  // Move the first board into the second
  const hAuto = await workspacePage.page.getByTitle("Fit content (Horizontal)");
  const vAuto = await workspacePage.page.getByTitle("Fit content (Vertical)");

  await expect(vAuto.locator("input")).toBeChecked();
  await expect(hAuto.locator("input")).toBeChecked();

  await workspacePage.moveSelectionToShape("Container Board");

  // The first board properties should still be auto width/height
  await expect(vAuto.locator("input")).toBeChecked();
  await expect(hAuto.locator("input")).toBeChecked();
});

test("BUG 9061 - Group blur visibility toggle icon not updating", async ({
  page,
}) => {
  const workspace = new WasmWorkspacePage(page);
  await workspace.setupEmptyFile();
  await workspace.mockRPC(/get\-file\?/, "design/get-file-9061.json");
  await workspace.mockRPC(
    "get-file-fragment?file-id=*&fragment-id=*",
    "design/get-file-fragment-9061.json",
  );
  await workspace.mockRPC("update-file?id=*", "design/update-file-9061.json");

  await workspace.goToWorkspace({
    fileId: "61cfa81d-8cb2-81df-8005-8f3005841116",
    pageId: "61cfa81d-8cb2-81df-8005-8f3005841117",
  });

  await workspace.clickLeafLayer("Group");

  const blurButton = workspace.page.getByRole("button", {
    name: "Toggle blur",
  });
  const blurIcon = blurButton.locator("svg use");
  await expect(blurIcon).toHaveAttribute("href", "#icon-shown");

  await blurButton.click();
  await expect(blurIcon).toHaveAttribute("href", "#icon-hide");
});

test.describe("Background blur", () => {
  test("Shows background blur option in blur type select when both render-wasm and background-blur flags are active", async ({
    page,
  }) => {
    const workspace = new WasmWorkspacePage(page);
    await workspace.mockConfigFlags([tokenInputFlag]);
    await workspace.setupEmptyFile();
    await workspace.mockGetFile("render-wasm/get-file-background-blur.json");

    await workspace.goToWorkspace({
      fileId: "93bfc923-66b2-813c-8007-b2725507ba08",
      pageId: "93bfc923-66b2-813c-8007-b2725507ba09",
    });

    // Click the first Rectangle (which has background-blur type)
    await workspace.clickLeafLayer("Rectangle");

    const blurSection = workspace.page.getByRole("region", {
      name: "Blur effects",
    });
    await expect(blurSection).toBeVisible();

    // The blur type select should show "Background blur" as the current value
    const blurTypeSelect = workspace.page.getByRole("combobox", {
      name: "Blur type select",
    });
    await expect(blurTypeSelect).toBeVisible();
    await expect(blurTypeSelect).toContainText("Background blur");

    // Select first group layer, which has not blur effect
    await workspace.layers.getByTestId("layer-row").nth(5).click();

    await expect(blurTypeSelect).not.toBeVisible();
    await blurSection.getByRole("button", { name: "Add blur" }).click();
    await expect(blurTypeSelect).toBeVisible();

    await expect(blurTypeSelect).toContainText("Layer blur");
  });

  test("Shows both layer-blur and background-blur options in the blur type dropdown", async ({
    page,
  }) => {
    const workspace = new WasmWorkspacePage(page);
    await workspace.mockConfigFlags([tokenInputFlag]);
    await workspace.setupEmptyFile();
    await workspace.mockGetFile("render-wasm/get-file-background-blur.json");

    await workspace.goToWorkspace({
      fileId: "93bfc923-66b2-813c-8007-b2725507ba08",
      pageId: "93bfc923-66b2-813c-8007-b2725507ba09",
    });

    await workspace.clickLeafLayer("Rectangle");
    const blurSection = workspace.page.getByRole("region", {
      name: "Blur effects",
    });
    await expect(blurSection).toBeVisible();

    // Open the blur type dropdown
    const blurTypeSelect = blurSection.getByRole("combobox", {
      name: "Blur type select",
    });
    await expect(blurTypeSelect).toBeVisible();

    await blurTypeSelect.click();

    // Both options should be visible
    const layerBlurOption = blurSection.getByRole("option", {
      name: "Layer blur",
    });
    const backgroundBlurOption = blurSection.getByRole("option", {
      name: "Background blur",
    });
    await expect(layerBlurOption).toBeVisible();
    await expect(backgroundBlurOption).toBeVisible();
  });

  test("Shape can have both layer blur and background blur effects at the same time", async ({
    page,
  }) => {
    const workspace = new WasmWorkspacePage(page);
    await workspace.mockConfigFlags([tokenInputFlag]);
    await workspace.setupEmptyFile();
    await workspace.mockGetFile("render-wasm/get-file-background-blur.json");

    await workspace.goToWorkspace({
      fileId: "93bfc923-66b2-813c-8007-b2725507ba08",
      pageId: "93bfc923-66b2-813c-8007-b2725507ba09",
    });

    await workspace.clickLeafLayer("Rectangle");
    const blurSection = workspace.page.getByRole("region", {
      name: "Blur effects",
    });
    await expect(blurSection).toBeVisible();

    const addBlurButton = blurSection.getByRole("button", { name: "Add blur" });
    await expect(addBlurButton).toBeVisible();
    await addBlurButton.click();

    const blurTypeSelect = blurSection.getByRole("combobox", {
      name: "Blur type select",
    });
    await expect(blurTypeSelect).toHaveCount(2);

    const backgroundBlurLabel = blurSection.getByText("Background blur");
    await expect(backgroundBlurLabel).toBeVisible();

    const layerBlurLabel = blurSection.getByText("Layer blur");
    await expect(layerBlurLabel).toBeVisible(); 
  });

  test("Show background blur disabled when flag is not active", async ({
    page,
  }) => {
    const workspace = new WasmWorkspacePage(page);
    // background-blur flag is active by default; disable it explicitly here
    await workspace.mockConfigFlags(["disable-background-blur"]);
    await workspace.setupEmptyFile();
    await workspace.mockGetFile("render-wasm/get-file-background-blur.json");

    await workspace.goToWorkspace({
      fileId: "93bfc923-66b2-813c-8007-b2725507ba08",
      pageId: "93bfc923-66b2-813c-8007-b2725507ba09",
    });

    await workspace.clickLeafLayer("Rectangle");

    // When there is no background blur flag the section has the old name "blur" instead of "blur effects"
    const blurSection = workspace.page.getByRole("region", {
      name: "Blur",
    });
    await expect(blurSection).toBeVisible();
    // Without the background-blur flag, no blur type dropdown should appear.
    // Instead, a plain "Background blur" label is shown and more option button is disabled.
    const blurTypeSelect = blurSection.getByRole("combobox", {
      name: "Blur type select",
    });
    await expect(blurTypeSelect).not.toBeVisible();

    const backgroundBlurLabel = blurSection.getByText("Background blur");
    await expect(backgroundBlurLabel).toBeVisible();

    const showMoreOptionsButton = blurSection.getByRole("button", {
      name: "Show/hide more options",
    });
    await expect(showMoreOptionsButton).toBeDisabled();

    const showAndHideButton = blurSection.getByRole("button", {
      name: "Toggle blur",
    });
    await expect(showAndHideButton).toBeDisabled();

    const addBlurButton = blurSection.getByRole("button", { name: "Add blur" });

    // We can add a layer blur, but not a background blur, and the type select should not appear
    await expect(addBlurButton).toBeVisible();
    await addBlurButton.click();

    await expect(blurTypeSelect).not.toBeVisible();
    await expect(backgroundBlurLabel).toBeVisible();

    const blurLabel = blurSection.getByText("Blur", { exact: true });
    await expect(blurLabel).toHaveCount(2);
  });
});

test.describe("Glass", () => {
  const openBlurFile = async (workspace) => {
    await workspace.setupEmptyFile();
    await workspace.mockGetFile("render-wasm/get-file-background-blur.json");
    await workspace.goToWorkspace({
      fileId: "93bfc923-66b2-813c-8007-b2725507ba08",
      pageId: "93bfc923-66b2-813c-8007-b2725507ba09",
    });
  };

  test("Glass is not offered when the glass flag is not active", async ({
    page,
  }) => {
    const workspace = new WasmWorkspacePage(page);
    await workspace.mockConfigFlags([tokenInputFlag]);
    await openBlurFile(workspace);

    await workspace.clickLeafLayer("Rectangle");
    const blurSection = workspace.page.getByRole("region", {
      name: "Blur effects",
    });
    await expect(blurSection).toBeVisible();

    await blurSection
      .getByRole("combobox", { name: "Blur type select" })
      .click();
    await expect(
      blurSection.getByRole("option", { name: "Background blur" }),
    ).toBeVisible();
    await expect(blurSection.getByRole("option", { name: "Glass" })).toHaveCount(
      0,
    );
  });

  test("Changes a background blur into glass and edits its options", async ({
    page,
  }) => {
    const workspace = new WasmWorkspacePage(page);
    await workspace.mockConfigFlags([tokenInputFlag, "enable-glass"]);
    await openBlurFile(workspace);

    await workspace.clickLeafLayer("Rectangle");
    const effectsSection = workspace.page.getByRole("region", {
      name: "Effects",
    });
    await expect(effectsSection).toBeVisible();

    const typeSelect = effectsSection.getByRole("combobox", {
      name: "Blur type select",
    });
    await typeSelect.click();
    await effectsSection.getByRole("option", { name: "Glass" }).click();
    await expect(typeSelect).toContainText("Glass");

    await effectsSection
      .getByRole("button", { name: "Show/hide more options" })
      .click();

    const glassOptions = effectsSection.getByTestId("glass-options");
    await expect(glassOptions).toBeVisible();

    // Clicking right of the pad center points the light at 90 degrees.
    const lightPad = glassOptions.getByRole("slider", { name: "Light angle" });
    await expect(lightPad).toBeVisible();
    const padBox = await lightPad.boundingBox();
    await lightPad.click({
      position: { x: padBox.width - 4, y: padBox.height / 2 },
    });
    await expect(lightPad).toHaveAttribute("aria-valuenow", "90");
    await expect(
      glassOptions.getByRole("textbox", { name: "Light angle" }),
    ).toHaveValue("90");

    const refraction = glassOptions.getByRole("textbox", {
      name: "Refraction",
    });
    await expect(refraction).toHaveValue("80");
    await refraction.fill("35");
    await refraction.press("Enter");
    await expect(
      glassOptions.getByRole("slider", { name: "Refraction" }),
    ).toHaveValue("35");

    await expect(
      glassOptions.getByRole("textbox", { name: "Highlight width" }),
    ).toHaveValue("2");
    await expect(
      glassOptions
        .getByTestId("glass-light-color")
        .getByRole("textbox", { name: "Color" }),
    ).toHaveValue("FFFFFF");

    // The advanced block starts closed while its values are the defaults.
    const saturation = glassOptions.getByRole("textbox", {
      name: "Saturation",
    });
    await expect(saturation).toHaveCount(0);
    await glassOptions.getByRole("button", { name: "Advanced" }).click();
    await expect(saturation).toHaveValue("100");
    await saturation.fill("150");
    await saturation.press("Enter");
    await expect(
      glassOptions.getByRole("slider", { name: "Saturation" }),
    ).toHaveValue("150");

    // Texture controls only show for the reeded texture.
    const amount = glassOptions.getByRole("textbox", { name: "Amount" });
    await expect(amount).toHaveCount(0);
    const texture = glassOptions.getByRole("combobox", { name: "Texture" });
    await texture.click();
    await glassOptions.getByRole("option", { name: "Reeded" }).click();
    await expect(amount).toHaveValue("30");
    await expect(
      glassOptions.getByRole("textbox", { name: "Scale" }),
    ).toHaveValue("8");
    await expect(
      glassOptions.getByRole("textbox", { name: "Angle", exact: true }),
    ).toHaveValue("0");

    await texture.click();
    await glassOptions.getByRole("option", { name: "None" }).click();
    await expect(amount).toHaveCount(0);
  });

  test("Adds glass as the third effect of a shape", async ({ page }) => {
    const workspace = new WasmWorkspacePage(page);
    await workspace.mockConfigFlags([tokenInputFlag, "enable-glass"]);
    await openBlurFile(workspace);

    await workspace.clickLeafLayer("Rectangle");
    const effectsSection = workspace.page.getByRole("region", {
      name: "Effects",
    });
    const addButton = effectsSection.getByRole("button", { name: "Add blur" });

    // The rectangle has a background blur: add layer blur, then glass.
    await addButton.click();
    await addButton.click();

    const typeSelects = effectsSection.getByRole("combobox", {
      name: "Blur type select",
    });
    await expect(typeSelects).toHaveCount(3);
    await expect(effectsSection.getByText("Glass")).toBeVisible();
    await expect(addButton).not.toBeVisible();
  });
});

test("BUG 9543 - Layout padding inputs not showing 'mixed' when needed", async ({
  page,
}) => {
  const workspace = new WasmWorkspacePage(page);

  await workspace.setupEmptyFile();
  await workspace.mockRPC(/get\-file\?/, "design/get-file-9543.json");
  await workspace.mockRPC(
    "get-file-fragment?file-id=*&fragment-id=*",
    "design/get-file-fragment-9543.json",
  );
  await workspace.mockRPC("update-file?id=*", "design/update-file-9543.json");

  await workspace.goToWorkspace({
    fileId: "525a5d8b-028e-80e7-8005-aa6cad42f27d",
    pageId: "525a5d8b-028e-80e7-8005-aa6cad42f27e",
  });

  await workspace.clickLeafLayer("Board");
  let toggle = workspace.page.getByRole("button", {
    name: "Show 4 sided padding options",
  });

  await toggle.click();
  const topPaddingInput = workspace.page.getByRole("textbox", {
    name: "Top padding",
  });
  await topPaddingInput.fill("10");
  await topPaddingInput.press("Enter");
  await toggle.click();

  const verticalPaddingInput = await workspace.page.getByRole("textbox", {
    name: "Vertical padding",
  });
  await expect(verticalPaddingInput).toHaveValue("");
  await expect(verticalPaddingInput).toHaveAttribute("placeholder", "Mixed");
});

test("BUG 11177 - Font size input not showing 'mixed' when needed", async ({
  page,
}) => {
  const workspace = new WasmWorkspacePage(page);
  await workspace.setupEmptyFile();
  await workspace.mockRPC(/get\-file\?/, "design/get-file-11177.json");

  await workspace.goToWorkspace({
    fileId: "b3e5731a-c295-801d-8006-3fc33c3b1b13",
    pageId: "b3e5731a-c295-801d-8006-3fc33c3b1b14",
  });

  await workspace.clickLeafLayer("Ipsum");
  await workspace.clickLeafLayer("Lorem", { modifiers: ["Shift"] });

  await workspace.expectSelectedLayer("Ipsum");
  await workspace.expectSelectedLayer("Lorem");

  const fontSizeInput = workspace.page.getByLabel("Font size");

  await expect(fontSizeInput).toHaveValue("");
  await expect(fontSizeInput).toHaveAttribute("placeholder", "Mixed");
});

test("BUG 12287 Fix identical text fills not being added/removed", async ({
  page,
}) => {
  const workspace = new WasmWorkspacePage(page);
  await workspace.setupEmptyFile();
  await workspace.mockRPC(/get\-file\?/, "design/get-file-12287.json");

  await workspace.goToWorkspace({
    fileId: "4bdef584-e28a-8155-8006-f3f8a71b382e",
    pageId: "4bdef584-e28a-8155-8006-f3f8a71b382f",
  });

  await workspace.clickLeafLayer("Lorem ipsum");

  const addFillButton = workspace.page.getByRole("button", {
    name: "Add fill",
  });

  await addFillButton.click();
  await addFillButton.click();
  await addFillButton.click();
  await addFillButton.click();

  await expect(
    workspace.page.getByRole("button", { name: "#B1B2B5" }),
  ).toHaveCount(4);

  await workspace.page
    .getByRole("button", { name: "Remove color" })
    .first()
    .click();

  await expect(
    workspace.page.getByRole("button", { name: "#B1B2B5" }),
  ).toHaveCount(3);
});

test("BUG 12384 - Export crashing when exporting a board", async ({ page }) => {
  const workspace = new WasmWorkspacePage(page);
  await workspace.setupEmptyFile();
  await workspace.mockRPC(/get\-file\?/, "design/get-file-12384.json");

  let hasExportRequestBeenIntercepted = false;
  await workspace.page.route("**/api/export", (route) => {
    if (hasExportRequestBeenIntercepted) {
      route.continue();
      return;
    }

    hasExportRequestBeenIntercepted = true;
    const payload = route.request().postData();
    const parsedPayload = JSON.parse(payload);

    expect(parsedPayload["~:exports"]).toHaveLength(1);
    expect(parsedPayload["~:exports"][0]["~:file-id"]).toBe(
      "~ufa6ce865-34dd-80ac-8006-fe0dab5539a7",
    );
    expect(parsedPayload["~:exports"][0]["~:page-id"]).toBe(
      "~ufa6ce865-34dd-80ac-8006-fe0dab5539a8",
    );

    route.fulfill({
      status: 200,
      contentType: "application/json",
      response: {},
    });
  });

  await workspace.goToWorkspace({
    fileId: "fa6ce865-34dd-80ac-8006-fe0dab5539a7",
    pageId: "fa6ce865-34dd-80ac-8006-fe0dab5539a8",
  });

  await workspace.clickLeafLayer("Board");

  let exportRequest = workspace.page.waitForRequest("**/api/export");

  await workspace.rightSidebar
    .getByRole("button", { name: "Export 1 element" })
    .click();

  await exportRequest;
});
