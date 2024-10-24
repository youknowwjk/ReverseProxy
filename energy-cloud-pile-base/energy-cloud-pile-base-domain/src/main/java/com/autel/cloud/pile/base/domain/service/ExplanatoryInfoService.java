package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.base.http.vo.PageReqVO;
import com.autel.cloud.pile.base.dto.ExplanatoryInfoDTO;
import com.autel.cloud.pile.base.vo.ExplanatoryInfoVO;
import com.autel.cloud.pile.base.vo.app.PageDTO;

public interface ExplanatoryInfoService {
    Result<ExplanatoryInfoVO> saveOrUpdate(ExplanatoryInfoDTO explanatoryInfoDTO);

    Result<PageDTO<ExplanatoryInfoVO>> page(PageReqVO<ExplanatoryInfoDTO> explanatoryInfoSearchDTO);

    Result<PageDTO<ExplanatoryInfoVO>> searchConfigPages(PageReqVO<ExplanatoryInfoDTO> explanatoryInfoSearchDTO, String language);
}
