package com.autel.cloud.pile.base.domain.service.impl;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.convert.OpEvseBrandConvert;
import com.autel.cloud.pile.base.domain.repository.OpEvseBrandRepository;
import com.autel.cloud.pile.base.domain.service.OpEvseBrandService;
import com.autel.cloud.pile.base.vo.OpEvseBrandVO;
import lombok.extern.log4j.Log4j2;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@Log4j2
public class OpEvseBrandServiceImpl implements OpEvseBrandService {

    @Autowired
    private OpEvseBrandRepository opEvseBrandRepository;


    @Override
    public Result<List<OpEvseBrandVO>> list() {
        return Result.ofSucceed(OpEvseBrandConvert.toBrandVOs(opEvseBrandRepository.list()));
    }
}
