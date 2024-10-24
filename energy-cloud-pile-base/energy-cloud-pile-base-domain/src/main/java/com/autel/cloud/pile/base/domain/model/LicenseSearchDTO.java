package com.autel.cloud.pile.base.domain.model;

import com.autel.cloud.base.common.page.PageDTO;
import lombok.Data;

@Data
public class LicenseSearchDTO extends PageDTO {
    String orderId;
    String pileOrGunType;
}
