package com.autel.cloud.pile.base.domain.service;


import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.dto.OpEvseBrandModelDTO;
import com.autel.cloud.pile.base.dto.VerifyBrandNameAndProductModelDTO;
import com.autel.cloud.pile.base.vo.OpEvseBrandModelVO;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.List;

public interface OpEvseBrandModelService {

    /**
     * 产品信息录入
     */
    Result<OpEvseBrandModelVO> addOpEvseBrandModel(OpEvseBrandModelDTO opEvseBrandModelDTO);

    /**
     * 分页查询产品型号信息
     */
    Result<Page<OpEvseBrandModelVO>> pages(OpEvseBrandModelDTO opEvseBrandModelDTO);

    /**
     *列表查询（根据品牌条件）
     */
    Result<List<OpEvseBrandModelVO>> queryOpEvseBrandModelByBrand(Long brandId);

    /**
     *产品型号详情
     */
    Result<OpEvseBrandModelVO> opEvseBrandModelDetail(Long id);

    /**
     *编辑产品型号信息
     */
    Result<Boolean> updateOpEvseBrandModel(OpEvseBrandModelDTO opEvseBrandModelDTO);

    /**
     *删除产品型号信息
     */
    Result<Boolean> deleteOpEvseBrandModel(OpEvseBrandModelDTO opEvseBrandModelDTO);

    /**
     *批量导入产品型号信息
     */
    Result<Boolean> saveOpEvseBrandModelList(List<OpEvseBrandModelDTO> opEvseBrandModelDTOList);

    Result<Boolean> verifyBrandNameAndProductModel(VerifyBrandNameAndProductModelDTO verifyBrandNameAndProductModelDTO);

    Result<Boolean> uploadBrandModelModuleXls(MultipartFile multipartFile);

    /**
     * 三方桩导入模板下载
     *
     * @param request  request
     * @param response response
     * @return Result
     */
    Result<Void> downModuleResourceXls(HttpServletRequest request, HttpServletResponse response);

    Result<OpEvseBrandModelVO> queryByBrandNameAndProductModel(OpEvseBrandModelDTO opEvseBrandModelDTO);
}
