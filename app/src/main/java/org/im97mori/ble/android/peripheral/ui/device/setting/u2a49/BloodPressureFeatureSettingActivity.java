package org.im97mori.ble.android.peripheral.ui.device.setting.u2a49;

import static org.im97mori.ble.android.peripheral.utils.Utils.setTextDistinct;

import android.os.Bundle;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import androidx.core.view.MenuProvider;

import androidx.lifecycle.HasDefaultViewModelProviderFactory;
import androidx.lifecycle.ViewModelProvider;
import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.databinding.BloodPressureFeatureSettingActivityBinding;
import org.im97mori.ble.android.peripheral.utils.AfterTextChangedTextWatcher;
import org.im97mori.ble.android.peripheral.utils.AutoDisposeViewModelProvider;
import org.im97mori.stacklog.LogUtils;

import dagger.hilt.android.AndroidEntryPoint;

import javax.inject.Inject;
import java.util.function.Function;

@AndroidEntryPoint
public class BloodPressureFeatureSettingActivity extends AppCompatActivity {

    @Inject
    Function<HasDefaultViewModelProviderFactory, ViewModelProvider.Factory> viewModelProviderFactoryFunction;

    private BloodPressureFeatureSettingViewModel mViewModel;

    private BloodPressureFeatureSettingActivityBinding mBinding;

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        mViewModel = new AutoDisposeViewModelProvider(this, viewModelProviderFactoryFunction.apply(this)).get(BloodPressureFeatureSettingViewModel.class);

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
        mViewModel.observeResponseCodeErrorString(this, charSequence -> mBinding.responseCode.setError(charSequence));
        mBinding.responseCodeEdit.addTextChangedListener(new AfterTextChangedTextWatcher(editable
                -> mViewModel.updateResponseCode(editable)));

        mViewModel.observeResponseDelay(this, charSequence -> setTextDistinct(mBinding.responseDelayEdit, charSequence));
        mViewModel.observeResponseDelayErrorString(this, charSequence -> mBinding.responseDelay.setError(charSequence));
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

        mViewModel.observeMultipleBond(this, check -> mBinding.isMultipleBondSupported.setChecked(check));
        mBinding.isMultipleBondSupported.setOnCheckedChangeListener((buttonView, isChecked) -> mViewModel.updateMultipleBond(isChecked));

        mViewModel.observeSavedData(this, intent -> {
            setResult(RESULT_OK, intent);
            finish();
        });

        mBinding.topAppBar.addMenuProvider(new MenuProvider() {

            @Override
            public void onCreateMenu(@NonNull Menu menu, @NonNull MenuInflater menuInflater) {
                menu.findItem(R.id.save).setEnabled(mBinding.rootContainer.getVisibility() == View.VISIBLE);
            }

            @Override
            public boolean onMenuItemSelected(@NonNull MenuItem menuItem) {
                boolean result = false;
                if (menuItem.getItemId() == R.id.save) {
                    mViewModel.save(throwable
                            -> Toast.makeText(BloodPressureFeatureSettingActivity.this
                            , throwable.getMessage()
                            , Toast.LENGTH_SHORT).show());
                    result = true;
                }
                return result;
            }
        });
    }

    @Override
    protected void onStart() {
        super.onStart();
        mViewModel.observeSetup(getIntent()
                , () -> {
                    mBinding.rootContainer.setVisibility(View.VISIBLE);
                    mBinding.topAppBar.invalidateMenu();
                }
                , throwable -> LogUtils.stackLog(throwable.getMessage()));
    }

}
