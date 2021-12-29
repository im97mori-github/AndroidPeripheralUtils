package org.im97mori.ble.android.peripheral.ui.device.setting.u2a49;

import static org.im97mori.ble.android.peripheral.utils.Utils.setTextDistinct;

import android.os.Bundle;
import android.view.MenuItem;
import android.view.View;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.databinding.BloodPressureFeatureSettingActivityBinding;
import org.im97mori.ble.android.peripheral.ui.BaseActivity;
import org.im97mori.ble.android.peripheral.utils.AfterTextChangedTextWatcher;
import org.im97mori.ble.android.peripheral.utils.MockableViewModelProvider;
import org.im97mori.stacklog.LogUtils;

import dagger.hilt.android.AndroidEntryPoint;

@AndroidEntryPoint
public class BloodPressureFeatureSettingActivity extends BaseActivity {

    private BloodPressureFeatureSettingViewModel mViewModel;

    private BloodPressureFeatureSettingActivityBinding mBinding;

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        mViewModel = new MockableViewModelProvider(this).get(BloodPressureFeatureSettingViewModel.class);

        mBinding = BloodPressureFeatureSettingActivityBinding.inflate(getLayoutInflater());
        setContentView(mBinding.getRoot());

        mViewModel.observeIsErrorResponse(this, check -> {
            mBinding.isErrorResponse.setChecked(check);

            int visibility = check ? View.GONE : View.VISIBLE;
            mBinding.isBodyMovementDetectionSupported.setVisibility(visibility);
            mBinding.isCuffFitDetectionSupportSupported.setVisibility(visibility);
            mBinding.isIrregularPulseDetectionSupported.setVisibility(visibility);
            mBinding.isPulseRateRangeDetectionSupported.setVisibility(visibility);
            mBinding.isMeasurementPositionDetectionSupported.setVisibility(visibility);
            mBinding.isMultipleBondSupported.setVisibility(visibility);

            mBinding.responseCode.setVisibility(check ? View.VISIBLE : View.GONE);
        });
        mBinding.isErrorResponse.setOnCheckedChangeListener((buttonView, isChecked) -> mViewModel.updateIsErrorResponse(isChecked));

        mViewModel.observeResponseCode(this, charSequence -> setTextDistinct(mBinding.responseCodeEdit, charSequence));
        mViewModel.observeResponseCodeError(this, charSequence -> mBinding.responseCode.setError(charSequence));
        mBinding.responseCodeEdit.addTextChangedListener(new AfterTextChangedTextWatcher(editable
                -> mViewModel.updateResponseCode(editable)));

        mViewModel.observeResponseDelay(this, charSequence -> setTextDistinct(mBinding.responseDelayEdit, charSequence));
        mViewModel.observeResponseDelayError(this, charSequence -> mBinding.responseDelay.setError(charSequence));
        mBinding.responseDelayEdit.addTextChangedListener(new AfterTextChangedTextWatcher(editable
                -> mViewModel.updateResponseDelay(editable)));

        mViewModel.observeBodyMovementDetection(this, check -> mBinding.isBodyMovementDetectionSupported.setChecked(check));
        mBinding.isBodyMovementDetectionSupported.setOnCheckedChangeListener((buttonView, isChecked) -> mViewModel.updateBodyMovementDetection(isChecked));

        mViewModel.observeCuffFitDetection(this, check -> mBinding.isCuffFitDetectionSupportSupported.setChecked(check));
        mBinding.isCuffFitDetectionSupportSupported.setOnCheckedChangeListener((buttonView, isChecked) -> mViewModel.updateCuffFitDetection(isChecked));

        mViewModel.observeIrregularPulseDetection(this, check -> mBinding.isIrregularPulseDetectionSupported.setChecked(check));
        mBinding.isIrregularPulseDetectionSupported.setOnCheckedChangeListener((buttonView, isChecked) -> mViewModel.updateIrregularPulseDetection(isChecked));

        mViewModel.observePulseRateRangeDetection(this, check -> mBinding.isPulseRateRangeDetectionSupported.setChecked(check));
        mBinding.isPulseRateRangeDetectionSupported.setOnCheckedChangeListener((buttonView, isChecked) -> mViewModel.updatePulseRateRangeDetection(isChecked));

        mViewModel.observeMeasurementPositionDetection(this, check -> mBinding.isMeasurementPositionDetectionSupported.setChecked(check));
        mBinding.isMeasurementPositionDetectionSupported.setOnCheckedChangeListener((buttonView, isChecked) -> mViewModel.updateMeasurementPositionDetection(isChecked));

        mViewModel.observeMultipleBondDetection(this, check -> mBinding.isMultipleBondSupported.setChecked(check));
        mBinding.isMultipleBondSupported.setOnCheckedChangeListener((buttonView, isChecked) -> mViewModel.updateMultipleBondDetection(isChecked));

        mBinding.topAppBar.setOnMenuItemClickListener(this::onOptionsItemSelected);
    }

    @Override
    protected void onStart() {
        super.onStart();
        mDisposable.add(mViewModel.setup(getIntent())
                .subscribe(() -> mBinding.rootContainer.setVisibility(View.VISIBLE), throwable -> LogUtils.stackLog(throwable.getMessage())));
    }

    @Override
    public boolean onOptionsItemSelected(@NonNull MenuItem item) {
        boolean result;
        if (item.getItemId() == R.id.save) {
            mDisposable.add(mViewModel.save()
                    .subscribe(intent -> {
                        setResult(RESULT_OK, intent);
                        finish();
                    }, throwable -> Toast.makeText(this, throwable.getMessage(), Toast.LENGTH_SHORT).show()));
            result = true;
        } else {
            result = super.onOptionsItemSelected(item);
        }
        return result;
    }

}
