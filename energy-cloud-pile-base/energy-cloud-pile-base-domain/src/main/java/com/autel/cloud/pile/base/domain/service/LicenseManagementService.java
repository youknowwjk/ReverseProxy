package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.base.common.page.PageDTO;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.ordercenter.vo.ReturnOrderRespVO;
import com.autel.cloud.pile.base.dto.GetLicenseDetailPageByOrderIdDTO;
import com.autel.cloud.pile.base.dto.license.DoNotRemindAgainDTO;
import com.autel.cloud.pile.base.dto.license.LicenseDetailDTO;
import com.autel.cloud.pile.base.vo.license.LicenseDetailVO;
import com.autel.cloud.pile.base.vo.license.LicenseGroupListVO;
import com.autel.cloud.pile.base.vo.license.LicenseStatusCountByOrderIdVO;
import com.autel.cloud.pile.base.vo.license.LicenseStatusCountVO;
import com.autel.cloud.pile.base.vo.subscribe.refund.ReturnLicenseVO;
import com.autel.cloud.pile.base.vo.subscribe.refund.ReturnSkuCountVO;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;

import java.util.List;

/**
 * 许可证管理
 */
public interface LicenseManagementService {

    boolean doNotRemindAgainViewEnable();

    boolean doNotPromptAgainOperation(DoNotRemindAgainDTO doNotRemindAgainDTO);

    LicenseStatusCountVO getLicenseStatusCount(String skuCode,String sellerId);

    LicenseStatusCountByOrderIdVO getLicenseStatusCountByOrderId(String orderId);

    List<LicenseGroupListVO> getLicenseList(PageDTO pageDTO);

    List<LicenseGroupListVO> getLicenseListBySellerId(String sellerId);

    Page<LicenseDetailVO> getLicenseDetailPage(LicenseDetailDTO licenseDetailDTO);
    Page<LicenseDetailVO> getLicenseDetailPageByOrderId(GetLicenseDetailPageByOrderIdDTO getLicenseDetailPageByOrderIdDTO);

    Integer restoreLicenseStatus();

    List<ReturnSkuCountVO> getReturnSkuCountByOrderId(Long orderId);

    ReturnOrderRespVO ReturnLicense(ReturnLicenseVO returnLicenseVO);

    void returnResultSuccessHandle(String orderId, Long returnId, Integer returnStatus);

    LicenseStatusCountVO getStatusCountBySellerId();
}
