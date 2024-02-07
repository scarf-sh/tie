data CreatePackageRequest where
  CreatePackageRequest_application_json :: CreatePackage1 -> CreatePackageRequest
  CreatePackageRequest_text_xml :: CreatePackage1 -> CreatePackageRequest

data CreatePackageResponse (status :: Natural) body where
  CreatePackageResponse_200_application_json :: CreatePackageResponse 200
