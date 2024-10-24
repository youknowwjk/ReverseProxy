package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.base.common.page.PageDTO;
import com.autel.cloud.pile.base.vo.EvseDynamicPricingVO;
import com.autel.cloud.pile.base.vo.OcpiLocationVO;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * Author:   A19011
 * Description: OcpiLocationService
 * Date:     2022/6/20 15:33
 *
 * @Version 0.0.1-SNAPSHOT
 */
@Service
public interface OcpiLocationService {
    ResponseEntity<List<OcpiLocationVO>> getLocationList(String dateFrom, String dateTo, Integer offset, Integer limit, String locationLink, String cpoId);

    OcpiLocationVO getEvseLocation(String locationId, String evseUid);

    Boolean updateLocationOcpiEnalbed(String locationId, Boolean ocpiEnabled);

    Boolean queryLocationOcpiEnabled(String locationId);

    Boolean queryTariffOcpiEnabled(String tariffId);

    /**
     * @param pageDTO
     * @return
     * @function 提供清单页面供EMP拉取(OCPI Gireve)
     */
    Page<EvseDynamicPricingVO> page(PageDTO pageDTO);
}
