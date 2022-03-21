package org.im97mori.ble.android.peripheral.ui.device.setting.u1810;

import android.os.Bundle;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.widget.Toast;

import androidx.activity.result.ActivityResultLauncher;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import androidx.core.view.MenuProvider;

import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.databinding.BloodPressureServiceSettingActivityBinding;
import org.im97mori.ble.android.peripheral.ui.device.setting.u2a35.BloodPressureMeasurementLauncherContract;
import org.im97mori.ble.android.peripheral.ui.device.setting.u2a36.IntermediateCuffPressureLauncherContract;
import org.im97mori.ble.android.peripheral.ui.device.setting.u2a49.BloodPressureFeatureLauncherContract;
import org.im97mori.ble.android.peripheral.utils.AutoDisposeViewModelProvider;
import org.im97mori.stacklog.LogUtils;

import dagger.hilt.android.AndroidEntryPoint;

@AndroidEntryPoint
public class BloodPressureServiceSettingActivity extends AppCompatActivity {

    private BloodPressureServiceSettingViewModel mViewModel;

    private final ActivityResultLauncher<byte[]> mStartBloodPressureMeasurementSettingActivity = registerForActivityResult(new BloodPressureMeasurementLauncherContract()
            , result -> mViewModel.setBloodPressureMeasurementData(result));

    private final ActivityResultLauncher<byte[]> mStartIntermediateCuffPressureSettingActivity = registerForActivityResult(new IntermediateCuffPressureLauncherContract()
            , result -> mViewModel.setIntermediateCuffPressureData(result));

    private final ActivityResultLauncher<byte[]> mBloodPressureFeatureSettingActivity = registerForActivityResult(new BloodPressureFeatureLauncherContract()
            , result -> mViewModel.setBloodPressureFeatureData(result));

    private BloodPressureServiceSettingActivityBinding mBinding;

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        mViewModel = new AutoDisposeViewModelProvider(this).get(BloodPressureServiceSettingViewModel.class);

        mBinding = BloodPressureServiceSettingActivityBinding.inflate(getLayoutInflater());
        setContentView(mBinding.getRoot());

        mViewModel.observeHasBloodPressureMeasurementData(this, check -> mBinding.bloodPressureMeasurementCardView.setChecked(check));
        mViewModel.observeHasIntermediateCuffPressureData(this, check -> mBinding.intermediateCuffPressureCardView.setChecked(check));
        mViewModel.observeHasBloodPressureFeatureData(this, check -> mBinding.bloodPressureFeatureCardView.setChecked(check));

        mViewModel.observeBloodPressureMeasurementFlags(this, s -> mBinding.bloodPressureMeasurementFlags.setText(s));
        mViewModel.observeBloodPressureMeasurementSystolic(this, s -> mBinding.bloodPressureMeasurementSystolic.setText(s));
        mViewModel.observeBloodPressureMeasurementDiastolic(this, s -> mBinding.bloodPressureMeasurementDiastolic.setText(s));
        mViewModel.observeBloodPressureMeanArterialPressure(this, s -> mBinding.bloodPressureMeasurementMeanArterialPressure.setText(s));
        mViewModel.observeIsBloodPressureMeasurementTimeStampSupported(this, check -> {
            int visibility = check ? View.VISIBLE : View.GONE;
            mBinding.bloodPressureMeasurementTimeStampTitle.setVisibility(visibility);
            mBinding.bloodPressureMeasurementTimeStamp.setVisibility(visibility);
        });
        mViewModel.observeBloodPressureMeasurementTimeStamp(this, s -> mBinding.bloodPressureMeasurementTimeStamp.setText(s));
        mViewModel.observeIsBloodPressureMeasurementPulseRateSupported(this, check -> {
            int visibility = check ? View.VISIBLE : View.GONE;
            mBinding.bloodPressureMeasurementPulseRateTitle.setVisibility(visibility);
            mBinding.bloodPressureMeasurementPulseRate.setVisibility(visibility);
        });
        mViewModel.observeBloodPressureMeasurementPulseRate(this, s -> mBinding.bloodPressureMeasurementPulseRate.setText(s));
        mViewModel.observeIsBloodPressureMeasurementUserIdSupported(this, check -> {
            int visibility = check ? View.VISIBLE : View.GONE;
            mBinding.bloodPressureMeasurementUserIdTitle.setVisibility(visibility);
            mBinding.bloodPressureMeasurementUserId.setVisibility(visibility);
        });
        mViewModel.observeBloodPressureMeasurementUserId(this, s -> mBinding.bloodPressureMeasurementUserId.setText(s));
        mViewModel.observeIsBloodPressureMeasurementMeasurementStatusSupported(this, check -> {
            int visibility = check ? View.VISIBLE : View.GONE;
            mBinding.bloodPressureMeasurementMeasurementStatusTitle.setVisibility(visibility);
            mBinding.bloodPressureMeasurementMeasurementStatus.setVisibility(visibility);
        });
        mViewModel.observeBloodPressureMeasurementMeasurementStatus(this, s -> mBinding.bloodPressureMeasurementMeasurementStatus.setText(s));

        mViewModel.observeIsIntermediateCuffPressureSupported(this, check -> {
            mBinding.isIntermediateCuffPressureSupported.setChecked(check);
            mBinding.intermediateCuffPressureCardView.setVisibility(check ? View.VISIBLE : View.GONE);
        });
        mViewModel.observeIntermediateCuffPressureFlags(this, s -> mBinding.intermediateCuffPressureFlags.setText(s));
        mViewModel.observeIntermediateCuffPressureCurrentCuffPressure(this, s -> mBinding.intermediateCuffPressureCurrentCuffPressure.setText(s));
        mViewModel.observeIsIntermediateCuffPressureTimeStampSupported(this, check -> {
            int visibility = check ? View.VISIBLE : View.GONE;
            mBinding.intermediateCuffPressureTimeStampTitle.setVisibility(visibility);
            mBinding.intermediateCuffPressureTimeStamp.setVisibility(visibility);
        });
        mViewModel.observeIntermediateCuffPressureTimeStamp(this, s -> mBinding.intermediateCuffPressureTimeStamp.setText(s));
        mViewModel.observeIsIntermediateCuffPressurePulseRateSupported(this, check -> {
            int visibility = check ? View.VISIBLE : View.GONE;
            mBinding.intermediateCuffPressurePulseRateTitle.setVisibility(visibility);
            mBinding.intermediateCuffPressurePulseRate.setVisibility(visibility);
        });
        mViewModel.observeIntermediateCuffPressurePulseRate(this, s -> mBinding.intermediateCuffPressurePulseRate.setText(s));
        mViewModel.observeIsIntermediateCuffPressureUserIdSupported(this, check -> {
            int visibility = check ? View.VISIBLE : View.GONE;
            mBinding.intermediateCuffPressureUserIdTitle.setVisibility(visibility);
            mBinding.intermediateCuffPressureUserId.setVisibility(visibility);
        });
        mViewModel.observeIntermediateCuffPressureUserId(this, s -> mBinding.intermediateCuffPressureUserId.setText(s));
        mViewModel.observeIsIntermediateCuffPressureMeasurementStatusSupported(this, check -> {
            int visibility = check ? View.VISIBLE : View.GONE;
            mBinding.intermediateCuffPressureMeasurementStatusTitle.setVisibility(visibility);
            mBinding.intermediateCuffPressureMeasurementStatus.setVisibility(visibility);
        });
        mViewModel.observeIntermediateCuffPressureMeasurementStatus(this, s -> mBinding.intermediateCuffPressureMeasurementStatus.setText(s));

        mViewModel.observeBloodPressureFeature(this, s -> mBinding.bloodPressureFeature.setText(s));

        mBinding.bloodPressureMeasurementSettingButton.setOnClickListener(v -> mStartBloodPressureMeasurementSettingActivity.launch(mViewModel.getBloodPressureMeasurementData()));
        mBinding.isIntermediateCuffPressureSupported.setOnCheckedChangeListener((buttonView, isChecked) -> mViewModel.updateIsIntermediateCuffPressureSupported(isChecked));
        mBinding.intermediateCuffPressureSettingButton.setOnClickListener(v -> mStartIntermediateCuffPressureSettingActivity.launch(mViewModel.getIntermediateCuffPressureData()));
        mBinding.bloodPressureFeatureSettingButton.setOnClickListener(v -> mBloodPressureFeatureSettingActivity.launch(mViewModel.getBloodPressureFeatureData()));

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
                            -> Toast.makeText(BloodPressureServiceSettingActivity.this
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
