package org.im97mori.ble.android.peripheral.ui.device.setting.u1810;

import android.os.Bundle;
import android.view.MenuItem;
import android.view.View;
import android.widget.CheckBox;
import android.widget.TextView;

import androidx.activity.result.ActivityResultLauncher;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.ViewModelProvider;

import com.google.android.material.appbar.MaterialToolbar;
import com.google.android.material.card.MaterialCardView;

import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.AndroidPeripheralUtilsApplication;
import org.im97mori.ble.android.peripheral.ui.BaseActivity;
import org.im97mori.ble.android.peripheral.ui.device.setting.u2a35.BloodPressureMeasurementLauncherContract;
import org.im97mori.ble.android.peripheral.ui.device.setting.u2a36.IntermediateCuffPressureLauncherContract;
import org.im97mori.ble.android.peripheral.ui.device.setting.u2a49.BloodPressureFeatureLauncherContract;

import io.reactivex.rxjava3.android.schedulers.AndroidSchedulers;
import io.reactivex.rxjava3.schedulers.Schedulers;

public class BloodPressureServiceSettingActivity extends BaseActivity {

    private BloodPressureServiceSettingViewModel mViewModel;

    final ActivityResultLauncher<String> mStartBloodPressureMeasurementSettingActivity = registerForActivityResult(new BloodPressureMeasurementLauncherContract()
            , result -> mViewModel.setBloodPressureMeasurementDataString(result));

    final ActivityResultLauncher<String> mStartIntermediateCuffPressureSettingActivity = registerForActivityResult(new IntermediateCuffPressureLauncherContract()
            , result -> mViewModel.setIntermediateCuffPressureDataString(result));

    final ActivityResultLauncher<String> mBloodPressureFeatureSettingActivity = registerForActivityResult(new BloodPressureFeatureLauncherContract()
            , result -> mViewModel.setBloodPressureFeatureDataString(result));

    private MaterialCardView mBloodPressureMeasurementCardView;
    private TextView mBloodPressureMeasurementFlags;
    private TextView mBloodPressureMeasurementSystolic;
    private TextView mBloodPressureMeasurementDiastolic;
    private TextView mBloodPressureMeasurementMeanArterialPressure;
    private TextView mBloodPressureMeasurementTimeStampTitle;
    private TextView mBloodPressureMeasurementTimeStamp;
    private TextView mBloodPressureMeasurementPulseRateTitle;
    private TextView mBloodPressureMeasurementPulseRate;
    private TextView mBloodPressureMeasurementUserIdTitle;
    private TextView mBloodPressureMeasurementUserId;
    private TextView mBloodPressureMeasurementMeasurementStatusTitle;
    private TextView mBloodPressureMeasurementMeasurementStatus;

    private CheckBox mIntermediateCuffPressureSupported;
    private MaterialCardView mIntermediateCuffPressureCardView;
    private TextView mIntermediateCuffPressureFlags;
    private TextView mIntermediateCuffPressureCurrentCuffPressure;
    private TextView mIntermediateCuffPressureTimeStampTitle;
    private TextView mIntermediateCuffPressureTimeStamp;
    private TextView mIntermediateCuffPressurePulseRateTitle;
    private TextView mIntermediateCuffPressurePulseRate;
    private TextView mIntermediateCuffPressureUserIdTitle;
    private TextView mIntermediateCuffPressureUserId;
    private TextView mIntermediateCuffPressureMeasurementStatusTitle;
    private TextView mIntermediateCuffPressureMeasurementStatus;

    private MaterialCardView mBloodPressureFeatureCardView;
    private TextView mBloodPressureFeature;

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        mApplicationComponent = ((AndroidPeripheralUtilsApplication) getApplication()).getComponent();

        mApplicationComponent.inject(this);
        super.onCreate(savedInstanceState);
        mViewModel = new ViewModelProvider(this).get(BloodPressureServiceSettingViewModel.class);
        mApplicationComponent.inject(mViewModel);

        setContentView(R.layout.blood_pressure_service_setting_activity);

        mBloodPressureMeasurementCardView = findViewById(R.id.bloodPressureMeasurementCardView);
        mBloodPressureMeasurementFlags = findViewById(R.id.bloodPressureMeasurementFlags);
        mBloodPressureMeasurementSystolic = findViewById(R.id.bloodPressureMeasurementSystolic);
        mBloodPressureMeasurementDiastolic = findViewById(R.id.bloodPressureMeasurementDiastolic);
        mBloodPressureMeasurementMeanArterialPressure = findViewById(R.id.bloodPressureMeasurementMeanArterialPressure);
        mBloodPressureMeasurementTimeStampTitle = findViewById(R.id.bloodPressureMeasurementTimeStampTitle);
        mBloodPressureMeasurementTimeStamp = findViewById(R.id.bloodPressureMeasurementTimeStamp);
        mBloodPressureMeasurementPulseRateTitle = findViewById(R.id.bloodPressureMeasurementPulseRateTitle);
        mBloodPressureMeasurementPulseRate = findViewById(R.id.bloodPressureMeasurementPulseRate);
        mBloodPressureMeasurementUserIdTitle = findViewById(R.id.bloodPressureMeasurementUserIdTitle);
        mBloodPressureMeasurementUserId = findViewById(R.id.bloodPressureMeasurementUserId);
        mBloodPressureMeasurementMeasurementStatusTitle = findViewById(R.id.bloodPressureMeasurementMeasurementStatusTitle);
        mBloodPressureMeasurementMeasurementStatus = findViewById(R.id.bloodPressureMeasurementMeasurementStatus);

        mIntermediateCuffPressureSupported = findViewById(R.id.intermediateCuffPressureSupported);
        mIntermediateCuffPressureCardView = findViewById(R.id.intermediateCuffPressureCardView);
        mIntermediateCuffPressureFlags = findViewById(R.id.intermediateCuffPressureFlags);
        mIntermediateCuffPressureCurrentCuffPressure = findViewById(R.id.intermediateCuffPressureSystolic);
        mIntermediateCuffPressureTimeStampTitle = findViewById(R.id.intermediateCuffPressureTimeStampTitle);
        mIntermediateCuffPressureTimeStamp = findViewById(R.id.intermediateCuffPressureTimeStamp);
        mIntermediateCuffPressurePulseRateTitle = findViewById(R.id.intermediateCuffPressurePulseRateTitle);
        mIntermediateCuffPressurePulseRate = findViewById(R.id.intermediateCuffPressurePulseRate);
        mIntermediateCuffPressureUserIdTitle = findViewById(R.id.intermediateCuffPressureUserIdTitle);
        mIntermediateCuffPressureUserId = findViewById(R.id.intermediateCuffPressureUserId);
        mIntermediateCuffPressureMeasurementStatusTitle = findViewById(R.id.intermediateCuffPressureMeasurementStatusTitle);
        mIntermediateCuffPressureMeasurementStatus = findViewById(R.id.intermediateCuffPressureMeasurementStatus);

        mBloodPressureFeatureCardView = findViewById(R.id.bloodPressureFeatureCardView);
        mBloodPressureFeature = findViewById(R.id.bloodPressureFeature);

        mViewModel.observeHasBloodPressureMeasurement(this, aBoolean -> mBloodPressureMeasurementCardView.setChecked(aBoolean));
        mViewModel.observeHasIntermediateCuffPressure(this, aBoolean -> mIntermediateCuffPressureCardView.setChecked(aBoolean));
        mViewModel.observeHasBloodPressureFeature(this, aBoolean -> mBloodPressureFeatureCardView.setChecked(aBoolean));

        mViewModel.observeBloodPressureMeasurementFlags(this, s -> mBloodPressureMeasurementFlags.setText(s));
        mViewModel.observeBloodPressureMeasurementSystolic(this, s -> mBloodPressureMeasurementSystolic.setText(s));
        mViewModel.observeBloodPressureMeasurementDiastolic(this, s -> mBloodPressureMeasurementDiastolic.setText(s));
        mViewModel.observeBloodPressureMeanArterialPressure(this, s -> mBloodPressureMeasurementMeanArterialPressure.setText(s));
        mViewModel.observeHasBloodPressureMeasurementTimeStamp(this, aBoolean -> {
            int visibility = aBoolean ? View.VISIBLE : View.GONE;
            mBloodPressureMeasurementTimeStampTitle.setVisibility(visibility);
            mBloodPressureMeasurementTimeStamp.setVisibility(visibility);
        });
        mViewModel.observeBloodPressureMeasurementTimeStamp(this, s -> mBloodPressureMeasurementTimeStamp.setText(s));
        mViewModel.observeHasBloodPressureMeasurementPulseRate(this, aBoolean -> {
            int visibility = aBoolean ? View.VISIBLE : View.GONE;
            mBloodPressureMeasurementPulseRateTitle.setVisibility(visibility);
            mBloodPressureMeasurementPulseRate.setVisibility(visibility);
        });
        mViewModel.observeBloodPressureMeasurementPulseRate(this, s -> mBloodPressureMeasurementPulseRate.setText(s));
        mViewModel.observeHasBloodPressureMeasurementUserId(this, aBoolean -> {
            int visibility = aBoolean ? View.VISIBLE : View.GONE;
            mBloodPressureMeasurementUserIdTitle.setVisibility(visibility);
            mBloodPressureMeasurementUserId.setVisibility(visibility);
        });
        mViewModel.observeBloodPressureMeasurementUserId(this, s -> mBloodPressureMeasurementUserId.setText(s));
        mViewModel.observeHasBloodPressureMeasuremenMeasurementStatus(this, aBoolean -> {
            int visibility = aBoolean ? View.VISIBLE : View.GONE;
            mBloodPressureMeasurementMeasurementStatusTitle.setVisibility(visibility);
            mBloodPressureMeasurementMeasurementStatus.setVisibility(visibility);
        });
        mViewModel.observeBloodPressureMeasurementMeasurementStatus(this, s -> mBloodPressureMeasurementMeasurementStatus.setText(s));


        mViewModel.observeSupportIntermediateCuffPressureMeasurement(this, aBoolean -> {
            if (aBoolean) {
                mIntermediateCuffPressureCardView.setVisibility(View.VISIBLE);
            } else {
                mIntermediateCuffPressureCardView.setChecked(false);
                mIntermediateCuffPressureCardView.setVisibility(View.GONE);
            }
            mIntermediateCuffPressureSupported.setChecked(aBoolean);
        });
        mViewModel.observeIntermediateCuffPressureFlags(this, s -> mIntermediateCuffPressureFlags.setText(s));
        mViewModel.observeIntermediateCuffPressureCurrentCuffPressure(this, s -> mIntermediateCuffPressureCurrentCuffPressure.setText(s));
        mViewModel.observeHasIntermediateCuffPressureTimeStamp(this, aBoolean -> {
            int visibility = aBoolean ? View.VISIBLE : View.GONE;
            mIntermediateCuffPressureTimeStampTitle.setVisibility(visibility);
            mIntermediateCuffPressureTimeStamp.setVisibility(visibility);
        });
        mViewModel.observeIntermediateCuffPressureTimeStamp(this, s -> mIntermediateCuffPressureTimeStamp.setText(s));
        mViewModel.observeHasIntermediateCuffPressurePulseRate(this, aBoolean -> {
            int visibility = aBoolean ? View.VISIBLE : View.GONE;
            mIntermediateCuffPressurePulseRateTitle.setVisibility(visibility);
            mIntermediateCuffPressurePulseRate.setVisibility(visibility);
        });
        mViewModel.observeIntermediateCuffPressurePulseRate(this, s -> mIntermediateCuffPressurePulseRate.setText(s));
        mViewModel.observeHasIntermediateCuffPressureUserId(this, aBoolean -> {
            int visibility = aBoolean ? View.VISIBLE : View.GONE;
            mIntermediateCuffPressureUserIdTitle.setVisibility(visibility);
            mIntermediateCuffPressureUserId.setVisibility(visibility);
        });
        mViewModel.observeIntermediateCuffPressureUserId(this, s -> mIntermediateCuffPressureUserId.setText(s));
        mViewModel.observeHasIntermediateCuffPressureMeasurementStatus(this, aBoolean -> {
            int visibility = aBoolean ? View.VISIBLE : View.GONE;
            mIntermediateCuffPressureMeasurementStatusTitle.setVisibility(visibility);
            mIntermediateCuffPressureMeasurementStatus.setVisibility(visibility);
        });
        mViewModel.observeIntermediateCuffPressureMeasurementStatus(this, s -> mIntermediateCuffPressureMeasurementStatus.setText(s));

        mViewModel.observeBloodPressureFeature(this, s -> mBloodPressureFeature.setText(s));

        findViewById(R.id.bloodPressureMeasurementSettingButton).setOnClickListener(v -> mStartBloodPressureMeasurementSettingActivity.launch(mViewModel.getBloodPressureMeasurementDataString()));
        mIntermediateCuffPressureSupported.setOnCheckedChangeListener((buttonView, isChecked) -> mViewModel.updateIntermediateCuffPressure(isChecked));
        findViewById(R.id.intermediateCuffPressureSettingButton).setOnClickListener(v -> mStartIntermediateCuffPressureSettingActivity.launch(mViewModel.getIntermediateCuffPressureDataString()));
        findViewById(R.id.bloodPressureFeatureSettingButton).setOnClickListener(v -> mBloodPressureFeatureSettingActivity.launch(mViewModel.getBloodPressureFeatureDataString()));

        MaterialToolbar bar = findViewById(R.id.topAppBar);
        bar.setOnMenuItemClickListener(this::onOptionsItemSelected);
    }

    @Override
    protected void onStart() {
        super.onStart();
        mDisposable.add(mViewModel.setup(getIntent())
                .subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(() -> findViewById(R.id.rootContainer).setVisibility(View.VISIBLE)));
    }

    @Override
    public boolean onOptionsItemSelected(@NonNull MenuItem item) {
        boolean result = false;
        if (item.getItemId() == R.id.save) {
            mDisposable.add(mViewModel.save()
                    .subscribeOn(Schedulers.io())
                    .observeOn(AndroidSchedulers.mainThread())
                    .subscribe(intent -> {
                                if (intent.isPresent()) {
                                    setResult(RESULT_OK, intent.get());
                                    finish();
                                }
                            }
                    ));
        } else {
            result = super.onOptionsItemSelected(item);
        }
        return result;
    }

}
